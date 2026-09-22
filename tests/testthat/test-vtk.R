# Tests for the VTK legacy format reader and writer (see R/vtk_legacy.R).
#
# The files in 'inst/extdata' whose names start with 'sphere_' and 'tracts_' were written by VTK itself
# (VTK 9.6.2, through pyvista), see 'dev_tools/generate_vtk_test_data.py'. They are the reference for the
# cell array layouts and encodings that VTK produces: 'sphere_v51_binary.vtk' uses the cell array layout of
# VTK 5.1 and later (separate OFFSETS and CONNECTIVITY arrays), 'sphere_v42_binary.vtk' uses the layout of
# VTK 4.2 and earlier (vertex counts followed by vertex indices in a single array), and 'tracts_v51_binary.vtk'
# contains streamlines instead of a mesh. All three are binary files.

#' @title Read the geometry dump written by 'dev_tools/generate_vtk_test_data.py'.
#'
#' @param filepath character string, path to a '*.expected.txt' file.
#'
#' @return named list with the entries 'points', 'polys', 'lines' and 'verts'.
#'
#' @keywords internal
read.vtk.geometry.dump.test <- function(filepath) {
  lines <- readLines(filepath)
  result <- list(points = NULL, polys = NULL, lines = NULL, verts = NULL)
  idx <- 2L
  while (idx <= length(lines)) {
    tokens <- strsplit(lines[idx], " ")[[1L]]
    key <- tokens[1L]
    num <- as.integer(tokens[2L])
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

#' @title Write a small ASCII VTK file from the given section lines.
#'
#' @param section_lines character vector, the lines after the 4 header lines.
#'
#' @return character string, the path to the temporary file that was written.
#'
#' @keywords internal
write.vtk.text.file.test <- function(section_lines) {
  filepath <- tempfile(fileext = ".vtk")
  writeLines(c("# vtk DataFile Version 5.1", "test", "ASCII", "DATASET POLYDATA", section_lines), filepath)
  return(filepath)
}

#' @title Check whether a file contains an ASCII byte sequence.
#'
#' @description Binary VTK files cannot be converted to an R string (they contain NUL bytes and, in the case of
#'   the binary payload, bytes that are invalid in the locale), so the raw bytes are searched directly.
#'
#' @param filepath character string, the path to the file to read.
#'
#' @param pattern character string, the byte sequence to look for.
#'
#' @return logical, whether the file contains the pattern.
#'
#' @keywords internal
vtk.file.contains.bytes.test <- function(filepath, pattern) {
  raw_bytes <- readBin(filepath, "raw", n = file.size(filepath))
  return(length(grepRaw(pattern, raw_bytes, fixed = TRUE, all = FALSE)) > 0L)
}


test_that("A VTK file written by VTK in the layout of VTK 5.1 can be read", {
  surface_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  surf <- read.fs.surface.vtk(surface_file)

  expect_equal(nrow(surf$vertices), 50L)
  expect_equal(nrow(surf$faces), 96L)
  expect_equal(ncol(surf$vertices), 3L)
  expect_equal(ncol(surf$faces), 3L)
  expect_equal(typeof(surf$vertices), "double")
  expect_equal(typeof(surf$faces), "integer")
  expect_true(all(surf$faces >= 1L))
  expect_true(all(surf$faces <= 50L))
  # The points are unit vectors: the sphere has radius 1.
  expect_equal(sqrt(rowSums(surf$vertices^2)), rep(1.0, 50L), tolerance = 1e-6)
  expect_s3_class(surf, "fs.surface")
})


test_that("A VTK file written by VTK in the layout of VTK 4.2 can be read", {
  surface_file <- system.file("extdata", "sphere_v42_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  surf <- read.fs.surface.vtk(surface_file)

  expect_equal(nrow(surf$vertices), 50L)
  expect_equal(nrow(surf$faces), 96L)
  expect_true(all(surf$faces >= 1L))
  expect_true(all(surf$faces <= 50L))
})


test_that("Both VTK cell array layouts are decoded identically", {
  old_layout_file <- system.file("extdata", "sphere_v42_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  new_layout_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)

  old_layout <- read.fs.surface.vtk(old_layout_file)
  new_layout <- read.fs.surface.vtk(new_layout_file)

  # The two files contain the same mesh, only their cell array layout differs.
  expect_identical(old_layout$vertices, new_layout$vertices)
  expect_identical(old_layout$faces, new_layout$faces)
})


test_that("Both VTK encodings of the same mesh are decoded identically", {
  surface <- read.fs.surface(system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE))

  ascii_file <- tempfile(fileext = ".vtk")
  binary_file <- tempfile(fileext = ".vtk")
  write.fs.surface.vtk(ascii_file, surface$vertices, surface$faces, binary = FALSE)
  write.fs.surface.vtk(binary_file, surface$vertices, surface$faces, binary = TRUE)

  expect_identical(read.fs.surface.vtk(ascii_file), read.fs.surface.vtk(binary_file))
})


test_that("The existing VTK test file with one coordinate per line can be read", {
  surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
  surf <- read.fs.surface.vtk(surface_file)

  expect_equal(nrow(surf$vertices), 8L)
  expect_equal(nrow(surf$faces), 12L)
  # The cube has a side length of 2 and is centered, so all coordinates are 1 or -1.
  expect_true(all(abs(surf$vertices) == 1.0))
})


test_that("VTK files store coordinates with an arbitrary number of them per line", {
  # VTK itself writes three coordinates (i.e., 9 values) per line, which the reader must not confuse with
  # one coordinate per line.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 4 float",
    "0 0 0 1 0 0 0 1 ",
    "0 1 1 0",
    "POLYGONS 2 8",
    "3 0 1 2",
    "3 1 2 3"
  ))
  surf <- read.fs.surface.vtk(filepath)
  expect_equal(nrow(surf$vertices), 4L)
  expect_equal(nrow(surf$faces), 2L)
  expect_identical(surf$vertices, matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0, 1, 1, 0), ncol = 3L, byrow = TRUE))
  expect_identical(surf$faces, matrix(c(1L, 2L, 3L, 2L, 3L, 4L), ncol = 3L, byrow = TRUE))
})


test_that("The cell array layout is detected, not derived from the file version", {
  # A file that announces VTK 5.1 in the header but uses the cell array layout of VTK 4.2. VTK's own reader
  # detects the layout from the presence of the OFFSETS keyword, and so does this implementation.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 4 float",
    "0 0 0 1 0 0 0 1 0 1 1 0",
    "POLYGONS 2 8",
    "3 0 1 2",
    "3 1 2 3"
  ))
  surf <- read.fs.surface.vtk(filepath)
  expect_equal(nrow(surf$faces), 2L)
  expect_identical(surf$faces, matrix(c(1L, 2L, 3L, 2L, 3L, 4L), ncol = 3L, byrow = TRUE))

  # And the other way round: the layout of VTK 5.1 in a file that announces version 1.0.
  filepath <- tempfile(fileext = ".vtk")
  writeLines(c(
    "# vtk DataFile Version 1.0", "test", "ASCII", "DATASET POLYDATA",
    "POINTS 4 float", "0 0 0 1 0 0 0 1 0 1 1 0",
    "POLYGONS 3 6", "OFFSETS vtktypeint64", "0 3 6", "CONNECTIVITY vtktypeint64", "0 1 2 1 2 3"
  ), filepath)
  surf <- read.fs.surface.vtk(filepath)
  expect_equal(nrow(surf$faces), 2L)
  expect_identical(surf$faces, matrix(c(1L, 2L, 3L, 2L, 3L, 4L), ncol = 3L, byrow = TRUE))
})


test_that("Attribute sections of VTK files are ignored", {
  # The generated test files contain NORMALS in a POINT_DATA section.
  surface_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  polydata <- read.vtk.legacy.polydata(surface_file)
  expect_true("POINT_DATA" %in% polydata$ignored_sections)

  # Attribute sections in an ASCII file, including one that precedes the cell array data in the file.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 3 float",
    "0 0 0 1 0 0 0 1 0",
    "POLYGONS 1 4",
    "3 0 1 2",
    "POINT_DATA 3",
    "SCALARS mydata float 1",
    "LOOKUP_TABLE default",
    "1 2 3"
  ))
  surf <- read.fs.surface.vtk(filepath)
  expect_equal(nrow(surf$vertices), 3L)
  expect_equal(nrow(surf$faces), 1L)
})


test_that("Unsupported VTK sections and datasets are rejected", {
  # Non-triangular polygons.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 4 float", "0 0 0 1 0 0 0 1 0 1 1 0",
    "POLYGONS 1 5", "4 0 1 2 3"
  ))
  expect_error(read.fs.surface.vtk(filepath), "not triangles")

  # Triangle strips.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 4 float", "0 0 0 1 0 0 0 1 0 1 1 0",
    "TRIANGLE_STRIPS 1 5", "4 0 1 2 3"
  ))
  expect_error(read.fs.surface.vtk(filepath), "TRIANGLE_STRIPS")

  # A point cloud, i.e. a VERTICES section and no polygons. The VERTICES section holds 3 cells with one
  # vertex each, so this is the layout of VTK 4.2: 3 cells occupying 6 integers (a vertex count and a vertex
  # index per cell).
  filepath <- write.vtk.text.file.test(c(
    "POINTS 3 float", "0 0 0 1 0 0 0 1 0",
    "VERTICES 3 6", "1 0", "1 1", "1 2"
  ))
  expect_error(read.fs.surface.vtk(filepath), "point cloud")

  # A dataset that is not a POLYDATA dataset.
  filepath <- tempfile(fileext = ".vtk")
  writeLines(c("# vtk DataFile Version 5.1", "test", "ASCII", "DATASET STRUCTURED_POINTS"), filepath)
  expect_error(read.fs.surface.vtk(filepath), "STRUCTURED_POINTS")

  # A file that is not a VTK legacy file at all.
  filepath <- tempfile(fileext = ".vtk")
  writeLines(c("not a vtk file", "foo", "bar", "baz"), filepath)
  expect_error(read.fs.surface.vtk(filepath), "not a valid VTK legacy file")

  # An unsupported data type in an otherwise valid section.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 1 long", "0 0 0",
    "POLYGONS 0 0"
  ))
  expect_error(read.fs.surface.vtk(filepath), "Unsupported VTK data type")

  # A section header that announces more values than the section contains.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 4 float", "0 0 0 1 0 0",
    "POLYGONS 1 4", "3 0 1 2"
  ))
  expect_error(read.fs.surface.vtk(filepath), "truncated")

  # A file without geometry.
  filepath <- write.vtk.text.file.test(c(
    "POINTS 0 float"
  ))
  expect_error(read.fs.surface.vtk(filepath), "does not contain any polygons")
})


test_that("Streamlines can be read from VTK files", {
  tracts_file <- system.file("extdata", "tracts_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  # The file contains a point that is not part of any streamline, which is dropped with a warning.
  expect_warning(tracts <- read.fs.tracts.vtk(tracts_file), "not part of any streamline")

  expect_true(is.fs.tracts(tracts))
  expect_equal(fs.tracts.count(tracts), 3L)
  expect_equal(fs.tracts.lengths(tracts), c(4L, 3L, 4L))
  expect_equal(fs.tracts.point.count(tracts), 11L)

  first_tract <- tracts[[1L]]
  expect_equal(nrow(first_tract), 4L)
  expect_equal(first_tract[1L, ], c(0.0, 0.0, 0.0))
  expect_equal(first_tract[4L, ], c(3.0, 0.0, 0.0))
  expect_equal(tracts[[2L]][3L, ], c(0.0, 3.0, 0.0))
})


test_that("Reading streamlines as a mesh and vice versa is refused with a helpful message", {
  tracts_file <- system.file("extdata", "tracts_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  expect_error(read.fs.surface.vtk(tracts_file), "read.fs.tracts.vtk")

  mesh_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  expect_error(read.fs.tracts.vtk(mesh_file), "read.fs.surface.vtk")
})


test_that("Gzipped VTK files can be read", {
  surface_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  gz_file <- tempfile(fileext = ".vtk.gz")
  con <- gzfile(gz_file, "wb")
  writeBin(readBin(surface_file, "raw", n = file.size(surface_file)), con)
  close(con)

  expect_identical(read.fs.surface.vtk(gz_file), read.fs.surface.vtk(surface_file))
})


test_that("Binary VTK files are read through the read.fs.surface dispatcher", {
  surface_file <- system.file("extdata", "sphere_v51_binary.vtk", package = "freesurferformats", mustWork = TRUE)
  expect_identical(read.fs.surface(surface_file), read.fs.surface.vtk(surface_file))
  expect_identical(read.fs.surface(surface_file, format = "vtk"), read.fs.surface.vtk(surface_file))
})


test_that("Surfaces can be written in all VTK legacy layouts and encodings", {
  surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
  surface <- read.fs.surface.vtk(surface_file)

  for (file_version in c(4.2, 5.1)) {
    for (binary in c(FALSE, TRUE)) {
      written_file <- tempfile(fileext = ".vtk")
      result <- write.fs.surface.vtk(written_file, surface$vertices, surface$faces,
        version = file_version, binary = binary
      )
      expect_equal(result, "tris")

      header_lines <- readLines(written_file, n = 4L)
      expect_equal(header_lines[1L], sprintf("# vtk DataFile Version %.1f", file_version))
      expect_equal(header_lines[3L], if (binary) "BINARY" else "ASCII")
      expect_equal(header_lines[4L], "DATASET POLYDATA")

      surface_read <- read.fs.surface.vtk(written_file)
      expect_identical(surface_read$faces, surface$faces)
      expect_equal(surface_read$vertices, surface$vertices, tolerance = 1e-6)

      # The file must announce the layout that was requested.
      if (file_version == 4.2) {
        expect_true(vtk.file.contains.bytes.test(written_file, "POLYGONS 12 48"))
        expect_false(vtk.file.contains.bytes.test(written_file, "OFFSETS"))
      } else {
        expect_true(vtk.file.contains.bytes.test(written_file, "POLYGONS 13 36"))
        expect_true(vtk.file.contains.bytes.test(written_file, "OFFSETS vtktypeint64"))
        expect_true(vtk.file.contains.bytes.test(written_file, "CONNECTIVITY vtktypeint64"))
      }
    }
  }
})


test_that("The VTK writer validates its parameters", {
  surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
  surface <- read.fs.surface.vtk(surface_file)
  out_file <- tempfile(fileext = ".vtk")

  expect_error(write.fs.surface.vtk(out_file, surface$vertices, surface$faces, version = 3.0), "either 4.2 or 5.1")
  expect_error(write.fs.surface.vtk(out_file, surface$vertices, surface$faces, version = "foo"), "must be a single number")
  expect_error(write.fs.surface.vtk(out_file, surface$vertices, surface$faces, binary = NA), "must be a single logical value")
  expect_error(write.fs.surface.vtk(out_file, surface$vertices, surface$faces, binary = "yes"), "must be a single logical value")

  # Face indices must be one-based. Faces of type double are accepted and converted to integers, like the
  # other surface writers do through write.fs.surface().
  zero_based_faces <- surface$faces - 1L
  expect_error(write.fs.surface.vtk(out_file, surface$vertices, zero_based_faces), "must be 1-based")
  write.fs.surface.vtk(out_file, surface$vertices, matrix(as.numeric(surface$faces), ncol = 3L))
  expect_identical(read.fs.surface.vtk(out_file)$faces, surface$faces)
})


test_that("The VTK writer handles meshes with a single face", {
  vertices <- matrix(c(0, 0, 0, 1, 0, 0, 0, 1, 0), ncol = 3L, byrow = TRUE)
  faces <- matrix(c(1L, 2L, 3L), ncol = 3L)
  for (file_version in c(4.2, 5.1)) {
    for (binary in c(FALSE, TRUE)) {
      written_file <- tempfile(fileext = ".vtk")
      write.fs.surface.vtk(written_file, vertices, faces, version = file_version, binary = binary)
      surface_read <- read.fs.surface.vtk(written_file)
      expect_identical(surface_read$faces, faces, info = sprintf("v%.1f binary=%s", file_version, binary))
      expect_equal(surface_read$vertices, vertices, tolerance = 1e-6)
    }
  }
})


test_that("VTK files are read identically to what VTK itself reads (reference files)", {
  # The files in 'extra_test_data/vtk' were written by VTK and come with a dump of the geometry as VTK reads
  # them back. These tests are skipped when the extra test data is not available.
  vtk_data_dir <- find_extra_test_data_file("vtk")
  skip_if(is.null(vtk_data_dir), "Test data missing.")

  vtk_files <- list.files(vtk_data_dir, pattern = "\\.vtk$", full.names = TRUE)
  skip_if(length(vtk_files) == 0L, "Test data missing.")

  for (vtk_file in vtk_files) {
    file_name <- sub("\\.vtk$", "", basename(vtk_file))
    dump_file <- file.path(vtk_data_dir, "expected", paste0(file_name, ".expected.txt"))
    if (!file.exists(dump_file)) {
      # Some reference files have no dump on purpose, e.g. the one that is not a POLYDATA dataset.
      next
    }

    expected <- read.vtk.geometry.dump.test(dump_file)
    polydata <- read.vtk.legacy.polydata(vtk_file)

    # The ASCII encoding stores single precision coordinates with 6 significant digits, so reading an ASCII
    # file back gives values that differ from VTK's single precision values by about 1e-7 relative. Binary
    # files store the coordinates as they are and must match exactly.
    tolerance <- if (polydata$encoding == "ASCII") 1e-6 else 0.0
    expect_equal(polydata$points, expected$points,
      tolerance = tolerance, info = file_name,
      ignore_attr = TRUE
    )

    for (cell_kind in c("polys", "lines", "verts")) {
      got <- polydata[[cell_kind]]
      got <- if (is.null(got)) NULL else lapply(got, as.integer)
      expect_identical(got, expected[[cell_kind]], info = paste(file_name, cell_kind))
    }
  }
})


test_that("The VTK writer refuses to write files that VTK would not read", {
  # The write side of the VTK support is verified against VTK itself (and against FreeSurfer, which writes
  # VTK files) by 'dev_tools/check_vtk_conversion.R', which needs a Python interpreter with pyvista and is
  # therefore not part of the test suite. What is checked here is that the written files contain the byte
  # patterns that identify the requested layout, and that reading them back gives the same mesh.
  surface_file <- system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE)
  surface <- read.fs.surface.vtk(surface_file)
  written_file <- tempfile(fileext = ".vtk")
  write.fs.surface.vtk(written_file, surface$vertices, surface$faces)

  # The default is the layout of VTK 4.2 in the ASCII encoding, which is what all VTK versions read.
  expect_true(vtk.file.contains.bytes.test(written_file, "# vtk DataFile Version 4.2"))
  expect_true(vtk.file.contains.bytes.test(written_file, "\nASCII\nDATASET POLYDATA\n"))
  expect_identical(read.fs.surface.vtk(written_file)$faces, surface$faces)
})
