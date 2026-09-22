test_that("One can write triangular surface data", {
  vertex_coords <- matrix(seq(1, 15) + 0.5, ncol = 3, byrow = TRUE)
  faces <- matrix(c(1L, 2L, 3L, 2L, 4L, 3L, 4L, 5L, 3L), nrow = 3, byrow = TRUE)

  format_written <- write.fs.surface(tempfile(fileext = "white"), vertex_coords, faces)
  expect_equal(format_written, "tris")
})

test_that("One can write and re-read triangular surface data", {
  vertex_coords <- matrix(seq(1, 15) + 0.5, nrow = 5, ncol = 3, byrow = TRUE)
  faces <- matrix(c(1L, 2L, 3L, 2L, 4L, 3L, 4L, 5L, 3L), nrow = 3, ncol = 3, byrow = TRUE)

  tmp_file <- tempfile(fileext = "white")
  format_written <- write.fs.surface(tmp_file, vertex_coords, faces)

  # Write a test file to a permanent location to manually check with freeview whether it gets read correctly ('freeview -f <file>').
  # format_written = write.fs.surface("/home/spirit/test.tiny.white", vertex_coords, faces);

  expect_equal(format_written, "tris")

  surf <- read.fs.surface(tmp_file)
  expect_equal(surf$internal$num_vertices_expected, 5)
  expect_equal(surf$internal$num_faces_expected, 3)
  expect_equal(nrow(surf$vertices), nrow(vertex_coords))

  expect_equal(typeof(surf$faces), "integer")
  expect_equal(typeof(surf$vertices), "double")

  expect_equal(nrow(surf$faces), nrow(faces))
  expect_equal(surf$mesh_face_type, "tris")

  expect_equal(surf$vertices, vertex_coords)
  expect_equal(surf$faces, faces)
})

test_that("One can read, write and re-read triangular surface data", {
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.")
  testthat::skip_on_cran() # cannot download testdata on CRAN.
  freesurferformats::download_opt_data()
  subjects_dir <- freesurferformats::get_opt_data_filepath("subjects_dir")
  surface_file <- file.path(subjects_dir, "subject1", "surf", "lh.white")

  skip_if_not(file.exists(surface_file), message = "Test data missing.")

  surf <- read.fs.surface(surface_file)

  tmp_file <- tempfile(fileext = "white")
  format_written <- write.fs.surface(tmp_file, surf$vertices, surf$faces)

  # One should also write the file to some permament location and manually ensure that freeview will open it correctly ('freeview -f <file>'):
  # format_written = write.fs.surface("/home/spirit/test.lh.white", surf$vertices, surf$faces);

  expect_equal(format_written, "tris")

  surf_re <- read.fs.surface(tmp_file)
  expect_equal(surf$internal$num_vertices_expected, surf_re$internal$num_vertices_expected)
  expect_equal(surf$internal$num_faces_expected, surf_re$internal$num_faces_expected)
  expect_equal(nrow(surf$vertices), nrow(surf_re$vertices))
  expect_equal(nrow(surf$faces), nrow(surf_re$faces))
  expect_equal(surf$mesh_face_type, "tris")
  expect_equal(surf$mesh_face_type, surf_re$mesh_face_type)

  expect_equal(surf$vertices, surf_re$vertices)
  expect_equal(surf$faces, surf_re$faces)
})


test_that("Surface files in VTK format can be read and written", {
  surface_file <- system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE)
  surf <- read.fs.surface(surface_file)

  tmp_vtk_file <- tempfile(fileext = ".vtk")
  write.fs.surface.vtk(tmp_vtk_file, surf$vertices, surf$faces)

  surf_re <- read.fs.surface(tmp_vtk_file)
  expect_equal(surf$vertices, surf_re$vertices)
  expect_equal(surf$faces, surf_re$faces)
})


test_that("One can export surface meshes in OFF, OBJ, PLY2 and PLY formats", {
  surface_file <- system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)

  # Standford PLY format without vertex colors
  write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices, mesh$faces)

  # PLY with vertex colors
  vertex_colors <- matrix(rep(82L, 5 * 4), ncol = 4) # the mesh contains 5 verts
  write.fs.surface.ply(tempfile(fileext = ".ply"), mesh$vertices, mesh$faces, vertex_colors = vertex_colors)

  # OFF, the Object File Format
  write.fs.surface.off(tempfile(fileext = ".off"), mesh$vertices, mesh$faces)

  # PLY2 format, very similar to OFF.
  write.fs.surface.ply2(tempfile(fileext = ".ply2"), mesh$vertices, mesh$faces)

  # Wavefront OBJ format
  write.fs.surface.obj(tempfile(fileext = ".obj"), mesh$vertices, mesh$faces)

  # FreeSurfer ASCII surface format
  write.fs.surface.asc(tempfile(fileext = ".fsascii"), mesh$vertices, mesh$faces)

  # GIFTI surface format
  write.fs.surface.gii(tempfile(fileext = ".gii"), mesh$vertices, mesh$faces)

  # MZ3 surface format
  write.fs.surface.mz3(tempfile(fileext = ".mz3"), mesh$vertices, mesh$faces)

  # BYU surface format
  write.fs.surface.byu(tempfile(fileext = ".byu"), mesh$vertices, mesh$faces)

  # currently this test only ensures that the functions run without error, the output is not checked in detail yet.
  # You can import the PLY and OBJ files into Blender, btw.
  expect_equal(1L, 1L)
})


test_that("One can export and re-read surface meshes in PLY format", {
  surface_file <- system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)

  # Standford PLY format without vertex colors
  ply_file <- tempfile(fileext = ".ply")
  write.fs.surface.ply(ply_file, mesh$vertices, mesh$faces)

  mesh_reread <- read.fs.surface.ply(ply_file)
  expect_equal(mesh$vertices, mesh_reread$vertices)
  expect_equal(mesh$faces, mesh_reread$faces)

  # PLY with vertex colors
  vertex_colors <- matrix(rep(82L, 5 * 4), ncol = 4) # the mesh contains 5 verts
  ply_col_file <- tempfile(fileext = ".ply")
  write.fs.surface.ply(ply_col_file, mesh$vertices, mesh$faces, vertex_colors = vertex_colors)

  col_mesh_reread <- read.fs.surface.ply(ply_col_file)
  expect_equal(mesh$vertices, col_mesh_reread$vertices)
  expect_equal(mesh$faces, col_mesh_reread$faces)
})


test_that("One can export and re-read surface meshes in STL format", {
  surface_file <- system.file("extdata", "cube.stl", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)
  num_faces <- nrow(mesh$faces)

  # The coordinates of every triangle, sorted, so that meshes can be compared
  # even though the vertex order of a polygon soup is reconstructed on reading.
  triangle_coords <- function(m) {
    t(apply(m$faces, 1L, function(f) sort(c(m$vertices[f[1L], ], m$vertices[f[2L], ], m$vertices[f[3L], ]))))
  }

  for (ascii in c(FALSE, TRUE)) {
    format_name <- if (ascii) "ASCII" else "binary"
    stl_file <- tempfile(fileext = if (ascii) ".stla" else ".stl")
    write.fs.surface.stl(stl_file, mesh$vertices, mesh$faces, ascii = ascii)

    # The binary version stores an 84 byte preamble and 50 bytes per face, the
    # ASCII version one 'solid' line, 7 lines per face and one 'endsolid' line.
    if (ascii) {
      expect_equal(length(readLines(stl_file)), 7L * num_faces + 2L, info = format_name)
    } else {
      expect_equal(file.size(stl_file), 84L + 50L * num_faces, info = format_name)
    }

    mesh_reread <- read.fs.surface.stl(stl_file)
    expect_equal(nrow(mesh_reread$vertices), nrow(mesh$vertices), info = format_name)
    expect_equal(nrow(mesh_reread$faces), num_faces, info = format_name)
    expect_equal(triangle_coords(mesh_reread), triangle_coords(mesh), tolerance = 1e-6, info = format_name)

    # The normals of the faces of a cube are the unit vectors along the axes.
    normals <- mesh_reread$metadata$normals
    expect_equal(dim(normals), c(num_faces, 3L), info = format_name)
    expect_equal(rep(1.0, num_faces), as.vector(abs(normals) %*% rep(1.0, 3L)), tolerance = 1e-6, info = format_name)
  }
})


test_that("The binary STL writer stores the documented record layout", {
  surface_file <- system.file("extdata", "cube.stl", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)

  stl_file <- tempfile(fileext = ".stl")
  write.fs.surface.stl(stl_file, mesh$vertices, mesh$faces)

  con <- file(stl_file, "rb")
  header_bytes <- readBin(con, "raw", n = 80L)
  face_count <- readBin(con, "integer", n = 1L, size = 4L, endian = "little")
  record <- readBin(con, "double", n = 12L, size = 4L, endian = "little")
  attr_count <- readBin(con, "integer", n = 1L, size = 2L, signed = FALSE, endian = "little")
  close(con)

  # The header must not start with the string 'solid': that is how readers, ours
  # included, tell the ASCII and the binary version of the format apart.
  expect_false(grepl("^solid", rawToChar(header_bytes), useBytes = TRUE))
  expect_equal(face_count, nrow(mesh$faces))
  expect_equal(attr_count, 0L)

  # The first record holds the normal and the 3 vertex coordinates of the first
  # face, in that order, and the normal is computed from the geometry.
  v <- mesh$vertices[mesh$faces[1L, ], ]
  edge1 <- v[2L, ] - v[1L, ]
  edge2 <- v[3L, ] - v[1L, ]
  normal <- c(edge1[2L] * edge2[3L] - edge1[3L] * edge2[2L],
              edge1[3L] * edge2[1L] - edge1[1L] * edge2[3L],
              edge1[1L] * edge2[2L] - edge1[2L] * edge2[1L])
  normal <- normal / sqrt(sum(normal^2))
  expect_equal(record[1:3], unname(normal), tolerance = 1e-6)
  expect_equal(record[4:12], as.vector(t(unname(v))), tolerance = 1e-6)

  # A degenerate triangle has no normal, it is written as a zero vector.
  degenerate <- tempfile(fileext = ".stl")
  write.fs.surface.stl(degenerate, matrix(c(0, 0, 0, 1, 1, 1, 2, 2, 2), ncol = 3L, byrow = TRUE),
                       matrix(c(1L, 2L, 3L), ncol = 3L))
  con <- file(degenerate, "rb")
  readBin(con, "raw", n = 84L)
  expect_equal(readBin(con, "double", n = 3L, size = 4L, endian = "little"), c(0, 0, 0))
  close(con)
})


test_that("The STL writer is reachable through write.fs.surface and checks its input", {
  surface_file <- system.file("extdata", "cube.stl", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)
  num_faces <- nrow(mesh$faces)

  # '.stl' and '.stlb' request the binary version, '.stla' the ASCII one.
  for (extension in c(".stl", ".stlb")) {
    fp <- tempfile(fileext = extension)
    expect_equal(write.fs.surface(fp, mesh$vertices, mesh$faces), "tris")
    expect_equal(file.size(fp), 84L + 50L * num_faces)
    expect_false(freesurferformats:::stl.format.file.is.ascii(fp))
  }

  fp_ascii <- tempfile(fileext = ".stla")
  expect_equal(write.fs.surface(fp_ascii, mesh$vertices, mesh$faces), "tris")
  expect_true(freesurferformats:::stl.format.file.is.ascii(fp_ascii))
  expect_equal(nrow(read.fs.surface(fp_ascii)$faces), num_faces)

  # The format can also be requested explicitly, for a file with any name.
  fp_explicit <- tempfile(fileext = ".dat")
  expect_equal(write.fs.surface(fp_explicit, mesh$vertices, mesh$faces, format = "stl"), "tris")
  expect_equal(file.size(fp_explicit), 84L + 50L * num_faces)

  # The format stores triangles only.
  quads <- cbind(mesh$faces, mesh$faces[, 1L])
  expect_error(write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, quads), "faces.quad.to.tris")
  expect_error(write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces - 1L), "1-based")
  expect_error(write.fs.surface.stl(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces, ascii = "yes"), "ascii")
})


test_that("One can write meshes in all formats directly from write.fs.surface", {
  surface_file <- system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE)
  mesh <- read.fs.surface(surface_file)

  # Standford PLY format
  write.fs.surface(tempfile(fileext = ".ply"), mesh$vertices, mesh$faces)

  # OFF, the Object File Format
  write.fs.surface(tempfile(fileext = ".off"), mesh$vertices, mesh$faces)

  # PLY2 format, very similar to OFF.
  write.fs.surface(tempfile(fileext = ".ply2"), mesh$vertices, mesh$faces)

  # Wavefront OBJ format
  write.fs.surface(tempfile(fileext = ".obj"), mesh$vertices, mesh$faces)

  # FreeSurfer ASCII surface format
  write.fs.surface(tempfile(fileext = ".asc"), mesh$vertices, mesh$faces)

  # GIFTI surface format
  write.fs.surface(tempfile(fileext = ".gii"), mesh$vertices, mesh$faces)

  # MZ3 surface format
  write.fs.surface(tempfile(fileext = ".mz3"), mesh$vertices, mesh$faces)

  # BYU surface format
  write.fs.surface(tempfile(fileext = ".byu"), mesh$vertices, mesh$faces)

  # VTK format
  write.fs.surface(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces)

  # STL format, the ASCII variant is requested by the file extension
  write.fs.surface(tempfile(fileext = ".stl"), mesh$vertices, mesh$faces)
  write.fs.surface(tempfile(fileext = ".stla"), mesh$vertices, mesh$faces)

  # error on invalid format
  expect_error(write.fs.surface(tempfile(fileext = ".vtk"), mesh$vertices, mesh$faces, format = "invalid format")) # invalid format
})
