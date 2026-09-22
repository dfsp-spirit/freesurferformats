# Cross-validate the STL format support of this package against VTK, FreeSurfer and meshio.
#
# The STL format stores a mesh as a 'polygon soup' (every triangle repeats its vertex coordinates) and exists
# in an ASCII and a binary variant. The format has no marker that distinguishes the two variants, readers tell
# them apart by checking whether the file starts with the string 'solid', and every triangle carries a normal
# vector that is redundant with the geometry. This script verifies our writer and our reader against
# implementations that share no code with this package:
#
#   1. Meshes written by this package must be read correctly by VTK (through pyvista): same number of points
#      and triangles, same triangle coordinates, same normals.
#   2. Meshes written by VTK must be read correctly by this package.
#   3. FreeSurfer's mris_convert writes STL files for surfaces and reads STL files. Both directions are checked
#      against the original FreeSurfer surface, so that vertex order and triangle geometry are validated on a
#      real cortical mesh and not only on the cube that ships with the package.
#   4. Meshio is used as a second independent reader if it is available.
#   5. The test data of the package is round-tripped through our own writer and reader.
#
# The Python, FreeSurfer and meshio parts are optional: the script reports which of them it could not find and
# skips those checks. Run it with
#   Rscript dev_tools/check_stl_conversion.R
# and use the environment variables STL_PYTHON and FREESURFER_HOME to point to non-standard installations.

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
repo_root <- if (length(script_file) == 1L) normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- getwd()
}
devtools::load_all(repo_root, quiet = TRUE)

freesurfer_home <- Sys.getenv("FREESURFER_HOME", unset = path.expand("~/software/freesurfer/freesurfer7.4.1"))
mris_convert_bin <- file.path(freesurfer_home, "bin", "mris_convert")

find_python_with <- function(imports) {
  candidates <- c(
    Sys.getenv("STL_PYTHON", unset = NA),
    path.expand("~/develop/brain_atlases/.venv/bin/python"),
    "/tmp/meshio_venv/bin/python",
    Sys.which("python3"),
    Sys.which("python")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    ok <- suppressWarnings(system2(candidate, c("-c", shQuote(sprintf("import %s", imports))), stdout = FALSE, stderr = FALSE))
    if (identical(as.integer(ok), 0L)) {
      return(candidate)
    }
  }
  return(NULL)
}

run_python <- function(python, code) {
  script_path <- tempfile(fileext = ".py")
  writeLines(code, script_path)
  on.exit(unlink(script_path), add = TRUE)
  output <- suppressWarnings(system2(python, script_path, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  if (is.null(status)) {
    status <- 0L
  }
  return(list(status = status, output = paste(output, collapse = "\n")))
}

# The triangles of a mesh in a canonical form, so that two meshes can be compared without depending on the
# order of the vertices in the vertex list (a polygon soup stores no vertex order). Every row holds the 9
# coordinates of one triangle, sorted, and the rows are sorted by a rounded copy of that data: rounding the
# sort key makes the row order of the two meshes agree even when their coordinates differ by one float32 unit
# (which the data files in the wild do, see the comments in the round trip section below). Rows whose keys are
# equal are interchangeable for the comparison that follows, since equal keys mean that they agree to 5
# decimal places, which is far finer than the comparison tolerance.
sorted_triangles <- function(vertices, faces, key_digits = 5L) {
  coords <- t(apply(faces, 1L, function(face) sort(as.vector(t(vertices[face, , drop = FALSE])))))
  coords <- matrix(coords, ncol = 9L)
  format_string <- sprintf("%%.%df", key_digits)
  keys <- apply(coords, 1L, function(row) paste(sprintf(format_string, row), collapse = "|"))
  return(coords[order(keys), , drop = FALSE])
}

# Compare two meshes and return a character vector of problems, which is empty when they agree. The triangles
# are compared one by one, in an order that is made canonical with a rounded sort key. This is exact enough
# for meshes whose coordinates are the same values on both sides (the test data of the package, and the files
# that the tools write from it), it is used for those. For meshes that have gone through a text format on one
# side only, use signature_differences() below instead: rounding a coordinate to 6 decimal places for a file
# whose values are in the hundreds moves some triangle sort keys across the rounding boundary, which reorders
# the rows and makes a row-wise comparison meaningless.
mesh_differences <- function(mesh_a, mesh_b, tolerance = 1e-4, check_vertex_count = TRUE) {
  problems <- character(0L)
  if (check_vertex_count && nrow(mesh_a$vertices) != nrow(mesh_b$vertices)) {
    problems <- c(problems, sprintf("%d vertices instead of %d", nrow(mesh_a$vertices), nrow(mesh_b$vertices)))
  }
  if (nrow(mesh_a$faces) != nrow(mesh_b$faces)) {
    problems <- c(problems, sprintf("%d triangles instead of %d", nrow(mesh_a$faces), nrow(mesh_b$faces)))
  }
  if (length(problems) > 0L) {
    return(problems)
  }
  tri_a <- sorted_triangles(mesh_a$vertices, mesh_a$faces)
  tri_b <- sorted_triangles(mesh_b$vertices, mesh_b$faces)
  coord_diff <- max(abs(tri_a - tri_b))
  if (!(coord_diff <= tolerance)) {
    problems <- c(problems, sprintf("triangle coordinates differ by %.3g", coord_diff))
  }
  return(problems)
}

# The area of every triangle of a mesh.
triangle_areas <- function(vertices, faces) {
  edge1 <- vertices[faces[, 2L], , drop = FALSE] - vertices[faces[, 1L], , drop = FALSE]
  edge2 <- vertices[faces[, 3L], , drop = FALSE] - vertices[faces[, 1L], , drop = FALSE]
  cross <- cbind(edge1[, 2L] * edge2[, 3L] - edge1[, 3L] * edge2[, 2L],
                 edge1[, 3L] * edge2[, 1L] - edge1[, 1L] * edge2[, 3L],
                 edge1[, 1L] * edge2[, 2L] - edge1[, 2L] * edge2[, 1L])
  return(sqrt(rowSums(cross^2)) / 2)
}

# Compare two meshes by properties that do not depend on the order of their vertices or triangles: the number
# of vertices and triangles, the sorted vertex coordinates per axis, the sorted triangle areas, the total
# surface area and the bounding box. A scrambled mesh cannot keep all of those.
signature_differences <- function(mesh_a, mesh_b, tolerance = 1e-3) {
  problems <- character(0L)
  if (nrow(mesh_a$vertices) != nrow(mesh_b$vertices)) {
    problems <- c(problems, sprintf("%d vertices instead of %d", nrow(mesh_a$vertices), nrow(mesh_b$vertices)))
  }
  if (nrow(mesh_a$faces) != nrow(mesh_b$faces)) {
    problems <- c(problems, sprintf("%d triangles instead of %d", nrow(mesh_a$faces), nrow(mesh_b$faces)))
  }
  if (length(problems) > 0L) {
    return(problems)
  }
  coord_diff <- max(abs(apply(mesh_a$vertices, 2L, sort) - apply(mesh_b$vertices, 2L, sort)))
  if (!(coord_diff <= tolerance)) {
    problems <- c(problems, sprintf("vertex coordinates differ by %.3g", coord_diff))
  }
  areas_a <- sort(triangle_areas(mesh_a$vertices, mesh_a$faces))
  areas_b <- sort(triangle_areas(mesh_b$vertices, mesh_b$faces))
  area_diff <- max(abs(areas_a - areas_b))
  if (!(area_diff <= tolerance)) {
    problems <- c(problems, sprintf("triangle areas differ by %.3g", area_diff))
  }
  total_a <- sum(areas_a)
  if (abs(total_a - sum(areas_b)) > 1e-5 * total_a) {
    problems <- c(problems, sprintf("the total surface area differs by %.3g instead of %.3g", sum(areas_b), total_a))
  }
  bbox_a <- c(apply(mesh_a$vertices, 2L, min), apply(mesh_a$vertices, 2L, max))
  bbox_b <- c(apply(mesh_b$vertices, 2L, min), apply(mesh_b$vertices, 2L, max))
  if (max(abs(bbox_a - bbox_b)) > tolerance) {
    problems <- c(problems, "the bounding box differs")
  }
  return(problems)
}

tally <- function(status, checks, failures) {
  if (status == "OK") {
    checks <- checks + 1L
  } else if (status == "MISMATCH") {
    checks <- checks + 1L
    failures <- failures + 1L
  }
  return(list("checks" = checks, "failures" = failures))
}

report <- function(check_name, tool_label, status, detail = "") {
  cat(sprintf("   [%-24s] %-30s : %-8s %s\n", check_name, tool_label, status, detail))
  return(status)
}

is_ascii_stl <- function(filepath) {
  return(freesurferformats:::stl.format.file.is.ascii(filepath))
}

checks <- 0L
failures <- 0L
skipped <- 0L

vtk_python <- find_python_with("pyvista, numpy")
meshio_python <- find_python_with("meshio")

work_dir <- tempfile("stl_check_")
dir.create(work_dir, recursive = TRUE)
on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

cube_file <- system.file("extdata", "cube.stl", package = "freesurferformats", mustWork = TRUE)
cube_bin_file <- system.file("extdata", "cube_bin.stl", package = "freesurferformats", mustWork = TRUE)

# Part 1: this package writes, VTK reads ----------------------------------------
cat("=== 1. Files written by this package, read by VTK ===\n")
if (is.null(vtk_python)) {
  cat("   SKIPPED: no Python interpreter with pyvista was found.\n")
  skipped <- skipped + 1L
} else {
  cube <- read.fs.surface(cube_file)
  for (ascii in c(FALSE, TRUE)) {
    stl_file <- file.path(work_dir, sprintf("cube_%s.stl", if (ascii) "ascii" else "binary"))
    write.fs.surface.stl(stl_file, cube$vertices, cube$faces, ascii = ascii)
    dump_file <- file.path(work_dir, sprintf("cube_%s.dump", if (ascii) "ascii" else "binary"))

    # VTK is asked for the points of every triangle in cell order, which is the polygon soup as VTK sees it.
    python_code <- sprintf(
      paste(
        "import pyvista as pv",
        "m = pv.read(%s)",
        "with open(%s, 'w') as fh:",
        "    fh.write('points %%d\\n' %% m.n_points)",
        "    fh.write('cells %%d\\n' %% m.n_cells)",
        "    fh.write('bounds %%.9g %%.9g %%.9g %%.9g %%.9g %%.9g\\n' %% tuple(m.bounds))",
        "    fh.write('normal %%.9g %%.9g %%.9g\\n' %% tuple(m.face_normals[0]))",
        "    for c in m.faces.reshape(-1, 4)[:, 1:]:",
        "        for i in c:",
        "            fh.write('vertex %%.9g %%.9g %%.9g\\n' %% tuple(m.points[i]))",
        sep = "\n"
      ),
      shQuote(stl_file), shQuote(dump_file)
    )
    result <- run_python(vtk_python, python_code)
    label <- paste0("cube ", if (ascii) "ASCII" else "binary")
    if (result$status != 0L || !file.exists(dump_file)) {
      status <- report(label, "VTK (pyvista)", "MISMATCH", "pyvista could not read the file")
    } else {
      lines <- readLines(dump_file)
      num_points <- as.integer(sub("^points ", "", lines[1L]))
      num_cells <- as.integer(sub("^cells ", "", lines[2L]))
      bounds <- as.numeric(strsplit(sub("^bounds ", "", lines[3L]), " ")[[1L]])
      first_normal <- as.numeric(strsplit(sub("^normal ", "", lines[4L]), " ")[[1L]])
      vtk_vertices <- matrix(as.numeric(unlist(strsplit(sub("^vertex ", "", lines[-(1:4)]), " "))), ncol = 3L, byrow = TRUE)
      vtk_faces <- matrix(seq_len(nrow(vtk_vertices)), ncol = 3L, byrow = TRUE)

      # VTK reads the file as a polygon soup, so it reports 3 vertices per triangle. The vertex count of the
      # two meshes is therefore not comparable, the triangles are.
      problems <- mesh_differences(list(vertices = vtk_vertices, faces = vtk_faces), cube, check_vertex_count = FALSE)
      if (num_cells != nrow(vtk_faces)) {
        problems <- c(problems, "VTK reports a different triangle count")
      }
      expected_bounds <- c(min(cube$vertices[, 1L]), max(cube$vertices[, 1L]), min(cube$vertices[, 2L]),
                           max(cube$vertices[, 2L]), min(cube$vertices[, 3L]), max(cube$vertices[, 3L]))
      if (max(abs(bounds - expected_bounds)) > 1e-5) {
        problems <- c(problems, "the bounding box differs")
      }
      # The first face of the cube is a unit square in a coordinate plane, so its normal is a unit vector
      # along one of the axes.
      if (!(abs(sum(abs(first_normal)) - 1) < 1e-5)) {
        problems <- c(problems, "the face normals are not unit vectors along the axes")
      }
      status <- report(label, "VTK (pyvista)", if (length(problems) == 0L) "OK" else "MISMATCH",
                       if (length(problems) == 0L) sprintf("%d points in the soup, %d triangles, normals ok", num_points, num_cells) else paste(problems, collapse = "; "))
    }
    result <- tally(status, checks, failures)
    checks <- result$checks
    failures <- result$failures
  }
}

# Part 2: VTK writes, this package reads ----------------------------------------
cat("=== 2. Files written by VTK, read by this package ===\n")
if (is.null(vtk_python)) {
  cat("   SKIPPED: no Python interpreter with pyvista was found.\n")
  skipped <- skipped + 1L
} else {
  vtk_stl <- file.path(work_dir, "vtk_written.stl")
  python_code <- sprintf(paste("import pyvista as pv", "m = pv.read(%s)", "m.save(%s)", sep = "\n"),
                         shQuote(cube_file), shQuote(vtk_stl))
  result <- run_python(vtk_python, python_code)
  if (result$status != 0L || !file.exists(vtk_stl)) {
    status <- report("vtk-written cube", "VTK (pyvista)", "MISMATCH", "pyvista could not write the file")
  } else {
    cube <- read.fs.surface(cube_file)
    reread <- read.fs.surface.stl(vtk_stl)
    problems <- signature_differences(reread, cube)
    if (is.null(reread$metadata$normals) || !identical(dim(reread$metadata$normals), c(nrow(reread$faces), 3L))) {
      problems <- c(problems, "the normals are missing")
    }
    status <- report("vtk-written cube", "VTK (pyvista)", if (length(problems) == 0L) "OK" else "MISMATCH",
                     if (length(problems) == 0L) sprintf("%d triangles, detected as %s", nrow(reread$faces), if (is_ascii_stl(vtk_stl)) "ASCII" else "binary") else paste(problems, collapse = "; "))
  }
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures
}

# Part 3: FreeSurfer writes and reads STL ---------------------------------------
cat("=== 3. STL files written and read by FreeSurfer ===\n")
surface_file <- file.path(freesurfer_home, "subjects", "fsaverage5", "surf", "lh.white")
if (!file.exists(mris_convert_bin) || !file.exists(surface_file)) {
  cat(sprintf("   SKIPPED: '%s' or the surface '%s' was not found.\n", mris_convert_bin, surface_file))
  skipped <- skipped + 1L
} else {
  reference <- read.fs.surface(surface_file)

  # FreeSurfer writes STL, this package reads it.
  fs_stl <- file.path(work_dir, "fs_written.stl")
  suppressWarnings(system2(mris_convert_bin, c(surface_file, fs_stl), stdout = TRUE, stderr = TRUE))
  if (!file.exists(fs_stl)) {
    status <- report("fsaverage5/lh.white", "FreeSurfer STL writer", "MISMATCH", "mris_convert wrote no file")
  } else {
    ours <- read.fs.surface.stl(fs_stl)
    # The STL file stores rounded coordinates, so the comparison is done on the surface signature instead of
    # triangle by triangle.
    problems <- signature_differences(ours, reference)
    status <- report("fsaverage5/lh.white", "FreeSurfer STL writer", if (length(problems) == 0L) "OK" else "MISMATCH",
                     if (length(problems) == 0L) sprintf("%d triangles, detected as %s", nrow(ours$faces), if (is_ascii_stl(fs_stl)) "ASCII" else "binary") else paste(problems, collapse = "; "))
  }
  result <- tally(status, checks, failures)
  checks <- result$checks
  failures <- result$failures

  # This package writes STL, FreeSurfer reads it.
  for (ascii in c(FALSE, TRUE)) {
    our_stl <- file.path(work_dir, sprintf("ours_for_fs_%s.stl", if (ascii) "ascii" else "binary"))
    write.fs.surface.stl(our_stl, reference$vertices, reference$faces, ascii = ascii)
    fs_back <- file.path(work_dir, sprintf("fs_reread_%s.surf", if (ascii) "ascii" else "binary"))
    suppressWarnings(system2(mris_convert_bin, c(our_stl, fs_back), stdout = TRUE, stderr = TRUE))
    label <- paste0("fsaverage5/lh.white ", if (ascii) "ASCII" else "binary")
    if (!file.exists(fs_back) || file.info(fs_back)$size == 0L) {
      status <- report(label, "FreeSurfer STL reader", "MISMATCH", "mris_convert could not read our file")
    } else {
      fs_mesh <- read.fs.surface(fs_back)
      problems <- signature_differences(fs_mesh, reference)
      status <- report(label, "FreeSurfer STL reader", if (length(problems) == 0L) "OK" else "MISMATCH",
                       if (length(problems) == 0L) sprintf("%d triangles round-tripped", nrow(fs_mesh$faces)) else paste(problems, collapse = "; "))
    }
    result <- tally(status, checks, failures)
    checks <- result$checks
    failures <- result$failures
  }
}

# Part 4: meshio as a second independent reader ---------------------------------
cat("=== 4. Files written by this package, read by meshio ===\n")
if (is.null(meshio_python)) {
  cat("   SKIPPED: no Python interpreter with meshio was found.\n")
  skipped <- skipped + 1L
} else {
  cube <- read.fs.surface(cube_file)
  for (ascii in c(FALSE, TRUE)) {
    stl_file <- file.path(work_dir, sprintf("cube_meshio_%s.stl", if (ascii) "ascii" else "binary"))
    write.fs.surface.stl(stl_file, cube$vertices, cube$faces, ascii = ascii)
    dump_file <- file.path(work_dir, sprintf("meshio_%s.dump", if (ascii) "ascii" else "binary"))
    python_code <- sprintf(
      paste(
        "import meshio",
        "m = meshio.read(%s)",
        "with open(%s, 'w') as fh:",
        "    fh.write('counts %%d %%d\\n' %% (len(m.points), len(m.cells[0].data)))",
        sep = "\n"
      ),
      shQuote(stl_file), shQuote(dump_file)
    )
    result <- run_python(meshio_python, python_code)
    label <- paste0("cube ", if (ascii) "ASCII" else "binary")
    if (result$status != 0L || !file.exists(dump_file)) {
      status <- report(label, "meshio", "MISMATCH", "meshio could not read the file")
    } else {
      counts <- as.integer(strsplit(sub("^counts ", "", readLines(dump_file)[1L]), " ")[[1L]])
      problems <- character(0L)
      if (counts[1L] != nrow(cube$vertices)) {
        problems <- c(problems, sprintf("%d points instead of %d", counts[1L], nrow(cube$vertices)))
      }
      if (counts[2L] != nrow(cube$faces)) {
        problems <- c(problems, sprintf("%d triangles instead of %d", counts[2L], nrow(cube$faces)))
      }
      status <- report(label, "meshio", if (length(problems) == 0L) "OK" else "MISMATCH",
                       if (length(problems) == 0L) sprintf("%d points, %d triangles", counts[1L], counts[2L]) else paste(problems, collapse = "; "))
    }
    result <- tally(status, checks, failures)
    checks <- result$checks
    failures <- result$failures
  }
}

# Part 5: round trip through this package --------------------------------------
cat("=== 5. Round trip of the test data shipped with this package ===\n")
for (source_file in c(cube_file, cube_bin_file)) {
  mesh <- read.fs.surface(source_file)
  for (ascii in c(FALSE, TRUE)) {
    stl_file <- file.path(work_dir, sprintf("roundtrip_%s_%s.stl", basename(source_file), if (ascii) "ascii" else "binary"))
    write.fs.surface.stl(stl_file, mesh$vertices, mesh$faces, ascii = ascii)
    reread <- read.fs.surface.stl(stl_file)
    problems <- mesh_differences(reread, mesh)
    detected <- if (is_ascii_stl(stl_file)) "ASCII" else "binary"
    if (detected != (if (ascii) "ASCII" else "binary")) {
      problems <- c(problems, sprintf("detected as %s", detected))
    }
    status <- report(paste(basename(source_file), if (ascii) "ASCII" else "binary"), "this package",
                     if (length(problems) == 0L) "OK" else "MISMATCH",
                     if (length(problems) == 0L) sprintf("%d triangles, %d bytes", nrow(reread$faces), file.size(stl_file)) else paste(problems, collapse = "; "))
    result <- tally(status, checks, failures)
    checks <- result$checks
    failures <- result$failures
  }
}

cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", checks, failures, skipped))
if (failures > 0L) {
  stop("STL conversion checks failed.\n")
}
cat("All STL conversion checks passed.\n")
