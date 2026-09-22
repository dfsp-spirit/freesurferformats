# Cross-validate the VTK legacy format support of this package against VTK itself and against FreeSurfer.
#
# The VTK legacy format does not record which cell array layout a file uses, nor the width of the integers of
# a binary file, and there is no specification of the format that has been updated since VTK 5.1 changed the
# cell array layout in 2015. Both properties therefore have to be detected from the file content, see the
# comment at the top of R/vtk_legacy.R. This script verifies that detection against reference implementations
# rather than against the package's own writer:
#
#   1. The test files in 'extra_test_data/vtk' were written by VTK (through pyvista), and every one of them is
#      accompanied by a '*.expected.txt' dump of the geometry as VTK itself reads it back. Reading them with
#      this package must reproduce those dumps.
#   2. Files written by this package must be readable by VTK, i.e. by an implementation that shares no code
#      with this package.
#   3. FreeSurfer's mris_convert writes VTK files for surfaces, and reading such a file with this package must
#      give the same mesh as reading the original FreeSurfer surface file.
#
# The Python and FreeSurfer parts are optional: the script reports which of them it could not find and skips
# those checks. Run it with
#   Rscript dev_tools/check_vtk_conversion.R
# and use the environment variables VTK_PYTHON and FREESURFER_HOME to point to non-standard installations. Use
#   python3 dev_tools/generate_vtk_test_data.py generate extra_test_data/vtk
# to recreate the test data (requires numpy and pyvista).

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
repo_root <- if (length(script_file) == 1L) normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- getwd()
}
devtools::load_all(repo_root, quiet = TRUE)

vtk_data_dir <- file.path(repo_root, "extra_test_data", "vtk")
generator_script <- file.path(repo_root, "dev_tools", "generate_vtk_test_data.py")
freesurfer_bin <- Sys.getenv("FREESURFER_HOME", unset = path.expand("~/software/freesurfer/freesurfer7.4.1"))
mris_convert_bin <- file.path(freesurfer_bin, "bin", "mris_convert")

# Find a Python interpreter that has pyvista (and hence VTK). Pyvista is used instead of the raw VTK Python
# bindings only because it makes the test data generation scripts short.
find_vtk_python <- function() {
  candidates <- c(
    Sys.getenv("VTK_PYTHON", unset = NA),
    path.expand("~/develop/brain_atlases/.venv/bin/python"),
    Sys.which("python3"),
    Sys.which("python")
  )
  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  for (candidate in candidates) {
    # system2() does not quote arguments, so the code passed to '-c' has to be quoted explicitly.
    ok <- suppressWarnings(system2(candidate, c("-c", shQuote("import pyvista, numpy")),
      stdout = FALSE, stderr = FALSE
    ))
    if (identical(as.integer(ok), 0L)) {
      return(candidate)
    }
  }
  return(NULL)
}

vtk_python <- find_vtk_python()

# Parse a '*.expected.txt' dump, i.e. the geometry of a VTK file as read by VTK itself.
read.geometry.dump <- function(filepath) {
  lines <- readLines(filepath)
  if (length(lines) < 1L || !startsWith(lines[1L], "vtk_test_data_dump")) {
    stop(sprintf("'%s' is not a geometry dump.\n", filepath))
  }
  result <- list(points = NULL, polys = NULL, lines = NULL, verts = NULL)
  idx <- 2L
  while (idx <= length(lines)) {
    tokens <- strsplit(lines[idx], " ")[[1L]]
    key <- tokens[1L]
    num <- as.integer(tokens[2L])
    if (is.na(num)) {
      stop(sprintf("Malformed line %d in geometry dump '%s'.\n", idx, filepath))
    }
    if (num < 0L) {
      idx <- idx + 1L
      next
    }
    values <- lines[(idx + 1L):(idx + num)]
    if (key == "points") {
      result$points <- matrix(as.numeric(unlist(strsplit(values, " "))), ncol = 3L, byrow = TRUE)
    } else {
      result[[key]] <- lapply(strsplit(values, " "), as.integer)
    }
    idx <- idx + num + 1L
  }
  return(result)
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
  cat(sprintf("   [%-22s] %-34s : %-8s %s\n", check_name, tool_label, status, detail))
  return(status)
}

checks <- 0L
failures <- 0L
skipped <- 0L


cat("=== 1. Reading files written by VTK, compared against VTK's own result ===\n")
if (!dir.exists(vtk_data_dir)) {
  cat(sprintf("   SKIPPED: test data directory '%s' does not exist. Regenerate it with:\n", vtk_data_dir))
  cat(sprintf("   %s %s generate extra_test_data/vtk\n", if (is.null(vtk_python)) "python3" else vtk_python, generator_script))
  skipped <- skipped + 1L
} else {
  vtk_files <- list.files(vtk_data_dir, pattern = "\\.vtk$", full.names = TRUE)
  for (vtk_file in vtk_files) {
    file_name <- sub("\\.vtk$", "", basename(vtk_file))
    dump_file <- file.path(vtk_data_dir, "expected", paste0(file_name, ".expected.txt"))
    if (!file.exists(dump_file)) {
      cat(sprintf("   [%-22s] no expected dump, skipped\n", file_name))
      skipped <- skipped + 1L
      next
    }
    expected <- read.geometry.dump(dump_file)
    polydata <- read.vtk.legacy.polydata(vtk_file)

    # The ASCII encoding stores the single precision coordinates with 6 significant digits, so reading an
    # ASCII file back gives values that differ from the single precision values VTK reports by ~1e-7 relative.
    # Binary files store the coordinates as they are and must match exactly.
    tolerance <- if (polydata$encoding == "ASCII") 1e-6 else 0.0

    problems <- character(0L)
    point_diff <- max(abs(polydata$points - expected$points))
    if (!isTRUE(all.equal(polydata$points, expected$points, tolerance = tolerance, check.attributes = FALSE))) {
      problems <- c(problems, sprintf("coordinates differ by %.3g", point_diff))
    }
    for (cell_kind in c("polys", "lines", "verts")) {
      got <- polydata[[cell_kind]]
      got <- if (is.null(got)) NULL else lapply(got, as.integer)
      if (!isTRUE(all.equal(got, expected[[cell_kind]]))) {
        problems <- c(problems, sprintf("%s differ", cell_kind))
      }
    }

    status <- if (length(problems) == 0L) "OK" else "MISMATCH"
    checks <- checks + 1L
    if (status == "MISMATCH") {
      failures <- failures + 1L
    }
    num_cells <- length(polydata$polys) + length(polydata$lines) + length(polydata$verts)
    cat(sprintf(
      "   [%-22s] %-34s : %-8s %d points, %d cells, max coordinate diff %.3g %s\n",
      file_name, paste0("VTK ", polydata$version, " ", polydata$encoding), status,
      nrow(polydata$points), num_cells, point_diff, paste(problems, collapse = "; ")
    ))
  }
}


cat("\n=== 1b. A binary file written by an old VTK version, read by this package ===\n")
# 'globe.vtk' ships with the pyvista examples and was written by a VTK version of the 4.2 era (the header
# reads '# vtk DataFile Version 4.2', it uses the old cell array layout and stores the coordinates as double
# precision values). It is the only file available here that was not written by a current VTK release, so it
# is the closest thing to a file from the wild that the reader can be tested against.
globe_file <- NULL
if (!is.null(vtk_python)) {
  examples_dir <- suppressWarnings(system2(vtk_python,
    c("-c", shQuote("import pyvista.examples, os; print(os.path.dirname(pyvista.examples.__file__))")),
    stdout = TRUE, stderr = FALSE
  ))
  if (length(examples_dir) > 0L) {
    candidate <- file.path(examples_dir[1L], "globe.vtk")
    if (file.exists(candidate)) {
      globe_file <- candidate
    }
  }
}
if (is.null(globe_file)) {
  cat("   SKIPPED: 'globe.vtk' from the pyvista examples was not found.\n")
  skipped <- skipped + 1L
} else {
  reference_dump <- tempfile(fileext = ".expected.txt")
  system2(vtk_python, c(generator_script, "read", globe_file, "--out", reference_dump),
    stdout = FALSE, stderr = FALSE
  )
  if (!file.exists(reference_dump) || file.info(reference_dump)$size == 0L) {
    cat("   SKIPPED: VTK could not read 'globe.vtk'.\n")
    skipped <- skipped + 1L
  } else {
    expected <- read.geometry.dump(reference_dump)
    polydata <- read.vtk.legacy.polydata(globe_file)
    problems <- character(0L)
    if (!isTRUE(all.equal(polydata$points, expected$points, tolerance = 0.0, check.attributes = FALSE))) {
      problems <- c(problems, sprintf("coordinates differ by %.3g", max(abs(polydata$points - expected$points))))
    }
    if (!identical(lapply(polydata$polys, as.integer), expected$polys)) {
      problems <- c(problems, "polys differ")
    }
    status <- if (length(problems) == 0L) "OK" else "MISMATCH"
    checks <- checks + 1L
    if (status == "MISMATCH") {
      failures <- failures + 1L
    }
    cat(sprintf(
      "   [%-22s] %-34s : %-8s %d points, %d cells, %s\n",
      "globe.vtk", paste0("old VTK ", polydata$version, " ", polydata$encoding), status,
      nrow(polydata$points), length(polydata$polys), paste(problems, collapse = "; ")
    ))
  }
}


cat("\n=== 2. Files written by this package, read back by VTK ===\n")
if (is.null(vtk_python)) {
  cat("   SKIPPED: no Python interpreter with pyvista was found, set VTK_PYTHON to use one.\n")
  skipped <- skipped + 1L
} else {
  work_dir <- tempfile("vtk_check_")
  dir.create(work_dir)

  cube_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
  test_meshes <- list(
    list(name = "cube", mesh = read.fs.surface.vtk(cube_file)),
    list(name = "tinysurface", mesh = read.fs.surface(system.file("extdata", "lh.tinysurface",
      package = "freesurferformats", mustWork = TRUE
    )))
  )

  for (test_mesh in test_meshes) {
    for (file_version in c(4.2, 5.1)) {
      for (binary in c(FALSE, TRUE)) {
        written_file <- file.path(work_dir, sprintf(
          "%s_v%.1f_%s.vtk", test_mesh$name, file_version, if (binary) "binary" else "ascii"
        ))
        write.fs.surface.vtk(written_file, test_mesh$mesh$vertices, test_mesh$mesh$faces,
          version = file_version, binary = binary
        )
        dump_file <- paste0(written_file, ".expected.txt")
        system2(vtk_python, c(generator_script, "read", written_file, "--out", dump_file),
          stdout = FALSE, stderr = FALSE
        )
        label <- sprintf("VTK %.1f %s", file_version, if (binary) "binary" else "ASCII")
        if (!file.exists(dump_file) || file.info(dump_file)$size == 0L) {
          report(test_mesh$name, label, "MISMATCH", "VTK could not read the written file")
          checks <- checks + 1L
          failures <- failures + 1L
          next
        }
        vtk_result <- read.geometry.dump(dump_file)
        problems <- character(0L)
        if (!isTRUE(all.equal(vtk_result$points, test_mesh$mesh$vertices, tolerance = 1e-6, check.attributes = FALSE))) {
          problems <- c(problems, "coordinates differ")
        }
        vtk_faces <- matrix(unlist(lapply(vtk_result$polys, function(x) x + 1L)), ncol = 3L, byrow = TRUE)
        if (!identical(vtk_faces, test_mesh$mesh$faces)) {
          problems <- c(problems, "faces differ")
        }
        status <- if (length(problems) == 0L) "OK" else "MISMATCH"
        checks <- checks + 1L
        if (status == "MISMATCH") {
          failures <- failures + 1L
        }
        cat(sprintf(
          "   [%-22s] %-34s : %-8s %s\n",
          test_mesh$name, label, status, paste(problems, collapse = "; ")
        ))
      }
    }
  }
}


cat("\n=== 3. Files written by FreeSurfer (mris_convert), read by this package ===\n")
# A real surface is used here: the tiny surfaces shipped in 'inst/extdata' are synthetic and degenerate, and
# mris_convert rewrites the coordinates of those. 'fsaverage5' is part of every FreeSurfer installation.
surface_file <- file.path(freesurfer_bin, "subjects", "fsaverage5", "surf", "lh.white")
if (!file.exists(mris_convert_bin) || !file.exists(surface_file)) {
  cat(sprintf(
    "   SKIPPED: '%s' and '%s' are needed here, set FREESURFER_HOME to point to a FreeSurfer installation.\n",
    mris_convert_bin, surface_file
  ))
  skipped <- skipped + 1L
} else {
  work_dir_fs <- tempfile("vtk_check_fs_")
  dir.create(work_dir_fs)
  fs_output_file <- file.path(work_dir_fs, "mris_convert.vtk")
  system2(mris_convert_bin, c(surface_file, fs_output_file), stdout = FALSE, stderr = FALSE)

  if (!file.exists(fs_output_file) || file.info(fs_output_file)$size == 0L) {
    report("mris_convert output", "VTK file from FreeSurfer", "MISMATCH", "mris_convert wrote no file")
    checks <- checks + 1L
    failures <- failures + 1L
  } else {
    reference <- read.fs.surface(surface_file)
    ours <- read.fs.surface.vtk(fs_output_file)
    coord_diff <- max(abs(ours$vertices - reference$vertices))
    problems <- character(0L)
    if (coord_diff > 1e-5) {
      problems <- c(problems, sprintf("coordinates differ by %.3g", coord_diff))
    }
    if (!identical(ours$faces, reference$faces)) {
      problems <- c(problems, "faces differ")
    }
    status <- if (length(problems) == 0L) "OK" else "MISMATCH"
    checks <- checks + 1L
    if (status == "MISMATCH") {
      failures <- failures + 1L
    }
    polydata <- read.vtk.legacy.polydata(fs_output_file)
    cat(sprintf(
      "   [%-22s] %-34s : %-8s %d points, max coordinate diff %.3g %s\n",
      "fsaverage5/lh.white", paste0("FreeSurfer VTK ", polydata$version, " ", polydata$encoding), status,
      nrow(ours$vertices), coord_diff, paste(problems, collapse = "; ")
    ))
  }
  unlink(work_dir_fs, recursive = TRUE)
}


cat(sprintf("\n%d checks performed, %d failures, %d skipped.\n", checks, failures, skipped))
if (failures > 0L) {
  stop("VTK conversion checks failed.\n")
}
cat("All VTK conversion checks passed.\n")
