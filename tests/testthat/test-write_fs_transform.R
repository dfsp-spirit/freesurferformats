# Tests for writing transformation files.

test_that("A transformation can be written as an LTA file and read back.", {
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(lta_file)
  out_file <- tempfile(fileext = ".lta")

  write.fs.transform.lta(tf, out_file)
  back <- read.fs.transform(out_file)

  expect_equal(back$matrix, tf$matrix)
  expect_equal(back$format, "lta")
  expect_equal(back$type, "0")
  expect_equal(back$space_in, "voxel")
  expect_equal(back$space_out, "voxel")
  expect_equal(back$voxel_base, 0L)

  # The volume geometry is reconstructed from the descriptors, which makes the file self-contained.
  expect_equal(back$src$dim, tf$src$dim)
  expect_equal(back$src$path, tf$src$path)
  expect_equal(back$src$vox2ras, tf$src$vox2ras)
  expect_equal(back$dst$vox2ras, tf$dst$vox2ras)
  expect_equal(back$dst$path, tf$dst$path)

  # The file is a text file with the structure FreeSurfer expects.
  file_lines <- readLines(out_file)
  expect_true(any(grepl("^type      = 0", file_lines)))
  expect_true(any(grepl("^nxforms   = 1", file_lines)))
  expect_true(any(grepl("^1 4 4", file_lines)))
  expect_true(any(grepl("^src volume info", file_lines)))
  expect_true(any(grepl("^dst volume info", file_lines)))
  expect_true(any(grepl("^cras   = ", file_lines)))

  expect_error(write.fs.transform(tf, out_file, format = "lta", nonsense = 1L)) # no such argument
  unlink(out_file)
})


test_that("The LTA writer only writes matrices the LTA format can describe.", {
  out_file <- tempfile(fileext = ".lta")

  # An LTA of type 1 maps RAS coordinates, so a voxel-to-voxel transformation cannot be written as one, and
  # vice versa.
  voxel_tf <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 0L, type = "1")
  expect_error(write.fs.transform.lta(voxel_tf, out_file), "type 1")
  ras_tf <- fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras", type = "0")
  expect_error(write.fs.transform.lta(ras_tf, out_file), "type 0")

  # A transformation with unknown spaces cannot be written, since the LTA type would be a guess.
  unknown_tf <- fs.transform(matrix = diag(4), space_in = NA_character_, space_out = NA_character_)
  expect_error(write.fs.transform.lta(unknown_tf, out_file), "spaces")

  # Without a type field the type follows from the spaces.
  ras_tf_no_type <- fs.transform(matrix = diag(4), space_in = "ras", space_out = "ras")
  write.fs.transform.lta(ras_tf_no_type, out_file)
  expect_equal(read.fs.transform(out_file)$type, "1")
  expect_equal(read.fs.transform(out_file)$space_in, "ras")

  # A transformation without volume geometry can still be written, since the caller can supply the volumes to
  # the tools that read it.
  geometry_free <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 0L)
  write.fs.transform.lta(geometry_free, out_file)
  back <- read.fs.transform(out_file)
  expect_equal(back$matrix, diag(4))
  expect_true(is.null(back$src))
  expect_true(is.null(back$dst))
  expect_error(write.fs.transform.lta("not a transform", out_file))
  unlink(out_file)
})


test_that("A transformation can be written as an xfm file and read back.", {
  xfm_file <- system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(xfm_file)
  out_file <- tempfile(fileext = ".xfm")

  write.fs.transform.xfm(tf, out_file)
  back <- read.fs.transform(out_file)

  expect_equal(back$matrix, tf$matrix)
  expect_equal(back$format, "xfm")
  expect_equal(back$type, "Linear")
  expect_equal(back$space_in, "ras")
  expect_equal(back$space_out, "ras")

  file_lines <- readLines(out_file)
  expect_equal(file_lines[1L], "MNI Transform File")
  expect_true(any(grepl("^Transform_Type = Linear;", file_lines)))
  expect_true(any(grepl(";$", file_lines))) # the last matrix line ends with a semicolon

  # Only affine transformations can be stored, since the format keeps three rows.
  non_affine <- diag(4L)
  non_affine[4L, ] <- c(0, 0, 1, 5)
  expect_error(write.fs.transform.xfm(fs.transform(matrix = non_affine, space_in = "ras", space_out = "ras"), out_file), "affine")

  # Only RAS to RAS transformations can be stored.
  voxel_tf <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 0L)
  expect_error(write.fs.transform.xfm(voxel_tf, out_file), "RAS")

  # An xfm refers to the RAS space of an image header, so a transformation in the FSL world space cannot be
  # written as one.
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  mat_file <- tempfile(fileext = ".mat")
  write.fs.transform(read.fs.transform(lta_file), mat_file, format = "fslmat")
  fsl_tf <- read.fs.transform(mat_file)
  src_volume <- read.fs.volume(system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  dst_volume <- read.fs.volume(system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE), with_header = TRUE)
  fsl_world <- transform2world(fsl_tf, src = src_volume, dst = dst_volume)
  expect_error(write.fs.transform.xfm(fsl_world, out_file), "frame")

  expect_error(write.fs.transform.xfm("not a transform", out_file))
  unlink(c(out_file, mat_file))
})


test_that("A transformation can be written as a tkregister dat file and read back.", {
  dat_file <- system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(dat_file)
  out_file <- tempfile(fileext = ".dat")

  write.fs.transform.dat(tf, out_file)
  back <- read.fs.transform(out_file)

  expect_equal(back$matrix, tf$matrix)
  expect_equal(back$format, "dat")
  expect_equal(back$space_in, "voxel")
  expect_equal(back$space_out, "ras")
  expect_equal(back$dst$frame, "tkreg")

  # The metadata of the file survives the round trip.
  expect_equal(back$subject, tf$subject)
  expect_equal(back$intensity, tf$intensity)
  expect_equal(back$in_plane_resolution, tf$in_plane_resolution)
  expect_equal(back$between_plane_resolution, tf$between_plane_resolution)
  expect_equal(length(readLines(out_file)), 9L)

  # The metadata can be set explicitly.
  write.fs.transform.dat(tf, out_file, subject = "my_subject", in_plane_resolution = c(3.0, 3.5))
  back2 <- read.fs.transform(out_file)
  expect_equal(back2$subject, "my_subject")
  expect_equal(c(back2$in_plane_resolution, back2$between_plane_resolution), c(3.0, 3.5))

  # A tkregister matrix maps voxel coordinates to RAS coordinates, nothing else.
  voxel_tf <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "voxel", voxel_base = 0L)
  expect_error(write.fs.transform.dat(voxel_tf, out_file), "tkregister")
  one_based <- fs.transform(matrix = diag(4), space_in = "voxel", space_out = "ras", voxel_base = 1L)
  expect_error(write.fs.transform.dat(one_based, out_file), "zero-based")
  expect_error(write.fs.transform.dat("not a transform", out_file))
  unlink(out_file)
})


test_that("Written matrices are read back exactly.", {
  # The writers use enough significant digits for an exact round trip of a double (at least 17, which is the
  # number that identifies a double uniquely). Fewer digits (15, as used by some other tools) lose precision.
  angle <- 0.1234567890123456
  rotation <- matrix(c(
    cos(angle), -sin(angle), 0, 0,
    sin(angle), cos(angle), 0, 0,
    0, 0, 1, 0,
    0, 0, 0, 1
  ), ncol = 4L, byrow = TRUE)
  # A value that is not exactly representable, to catch a lossy text format.
  rotation[1L, 4L] <- 1.0 / 3.0
  rotation[2L, 4L] <- -1e-7 / 7.0

  voxel_tf <- fs.transform(matrix = rotation, space_in = "voxel", space_out = "voxel", voxel_base = 0L)
  ras_tf <- fs.transform(matrix = rotation, space_in = "ras", space_out = "ras")

  for (format in c("lta", "dat", "xfm", "fslmat")) {
    # FSL matrix files use the '.mat' extension.
    extension <- if (format == "fslmat") "mat" else format
    out_file <- tempfile(fileext = paste0(".", extension))
    tf <- if (format == "dat") {
      fs.transform(matrix = rotation, space_in = "voxel", space_out = "ras", voxel_base = 0L)
    } else if (format == "lta" || format == "fslmat") {
      voxel_tf
    } else {
      ras_tf
    }
    write.fs.transform(tf, out_file)
    expect_identical(read.fs.transform(out_file)$matrix, rotation)
    unlink(out_file)
  }
})


test_that("A formatted value is read back exactly, on every platform.", {
  # The round trip of a value that is not exactly representable depends on the decimal conversion of the
  # platform: on macOS ARM64, 17 significant digits of -1e-7/7 are not enough (they are one unit in the last
  # place off), so the formatter verifies the round trip and writes more digits when it fails. This test
  # states the invariant that the writer relies on.
  values <- c(1.0 / 3.0, -1e-7 / 7.0, 0.1, pi, 1e-300, 2^-1074, .Machine$double.xmax, -0.0, 12345.6789)
  for (value in values) {
    text <- transform.value.text(value)
    expect_true(is.character(text) && length(text) == 1L)
    expect_equal(as.numeric(text), value, info = sprintf("value %s is written as '%s'", format(value), text))
  }

  # Non-finite values are written as such instead of failing.
  expect_equal(transform.value.text(Inf), "Inf")
  expect_equal(transform.value.text(NaN), "NaN")
  expect_equal(transform.values.text(c(1.0, 2.0)), "1 2")
})


test_that("The transformation writer dispatches on the format and the file extension.", {
  lta_file <- system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE)
  tf <- read.fs.transform(lta_file)

  # From the file extension.
  for (format in c("lta", "mat")) {
    out_file <- tempfile(fileext = paste0(".", format))
    write.fs.transform(tf, out_file)
    expect_equal(read.fs.transform(out_file)$matrix, tf$matrix)
    unlink(out_file)
  }

  # Explicitly, including from a file name without a useful extension.
  out_file <- tempfile()
  write.fs.transform(tf, out_file, format = "lta")
  expect_equal(read.fs.transform(out_file, format = "lta")$matrix, tf$matrix)

  expect_error(write.fs.transform(tf, tempfile(fileext = ".nii")))
  expect_error(write.fs.transform(tf, tempfile(), format = "nifti"), "not supported")
  expect_error(write.fs.transform("not a transform", tempfile(fileext = ".lta")))
  unlink(out_file)
})
