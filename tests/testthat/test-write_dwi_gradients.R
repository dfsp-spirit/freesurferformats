# Tests for writing diffusion MRI gradient tables: FSL bvecs/bvals pairs and
# MRtrix gradient tables. See R/write_dwi_gradients.R and the references listed
# there.


test_that("One can write and re-read a FSL-style bvec/bval pair", {
  bvec <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(-0.5, 0.5, 0.7071067))
  bval <- c(0, 1000, 1000, 2000)

  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bvec_file <- file.path(tmp_dir, "dwi.bvec")
  bval_file <- file.path(tmp_dir, "dwi.bval")

  expect_silent(write.dti.bvec(bvec_file, bvec))
  expect_silent(write.dti.bval(bval_file, bval))

  # the FSL layout is 3 lines for the components and a single line of b-values
  expect_equal(length(readLines(bvec_file)), 3L)
  expect_equal(length(readLines(bval_file)), 1L)

  expect_equal(unname(read.dti.bvec(bvec_file)), bvec, tolerance = 1e-12)
  expect_equal(read.dti.bval(bval_file), bval, tolerance = 1e-12)
})


test_that("The 'volumes' layout writes one volume per line", {
  bvec <- rbind(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1), c(0.5, 0.5, 0))
  bval <- c(0, 1000, 1000, 2000)

  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bvec_file <- file.path(tmp_dir, "bvecs")
  bval_file <- file.path(tmp_dir, "bvals")

  expect_silent(write.dti.bvec(bvec_file, bvec, layout = "volumes"))
  expect_silent(write.dti.bval(bval_file, bval, layout = "volumes"))

  expect_equal(length(readLines(bvec_file)), 4L)
  expect_equal(length(readLines(bval_file)), 4L)
  expect_equal(length(strsplit(readLines(bvec_file)[1], " ")[[1]]), 3L)

  expect_equal(unname(read.dti.bvec(bvec_file)), bvec, tolerance = 1e-12)
  expect_equal(read.dti.bval(bval_file), bval, tolerance = 1e-12)
})


test_that("One can write and re-read a gradient table in MRtrix format", {
  bvec <- rbind(c(0, 0, 0), c(1, 0, 0), c(0, 1, 0), c(-0.5, 0.5, 0.7071067))
  bval <- c(0, 1000, 1000, 2000)

  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  gt_file <- file.path(tmp_dir, "grad.b")

  expect_silent(write.dti.grad(gt_file, bvec, bval))
  expect_equal(length(readLines(gt_file)), 4L)
  expect_equal(length(strsplit(readLines(gt_file)[1], " ")[[1]]), 4L)

  expect_silent(gt <- read.dti.grad(gt_file))
  expect_equal(dim(gt), c(4L, 4L))
  expect_equal(colnames(gt), c("x", "y", "z", "b"))
  expect_equal(unname(gt[, 1:3]), bvec, tolerance = 1e-12)
  expect_equal(unname(gt[, 4]), bval, tolerance = 1e-12)

  # a full gradient table as returned by read.dti.grad can be written directly
  gt_file2 <- file.path(tmp_dir, "grad2.b")
  expect_silent(write.dti.grad(gt_file2, gt))
  expect_equal(readLines(gt_file), readLines(gt_file2))

  # the transposed variant is available, too. Its 4x4 shape is a square, so it
  # cannot be recognized automatically and the layout must be given.
  gt_file3 <- file.path(tmp_dir, "grad3.b")
  expect_silent(write.dti.grad(gt_file3, gt, layout = "components"))
  expect_equal(length(readLines(gt_file3)), 4L)
  expect_silent(gt3 <- read.dti.grad(gt_file3, layout = "components"))
  expect_equal(gt3, gt, tolerance = 1e-12)
})


test_that("Gradient values survive a round trip with full precision", {
  bvec <- rbind(c(0.0000123456789012345, -0.999999999999999, 1e-05), c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  bval <- c(0, 1000.125, 999.999999999, 2000)

  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bvec_file <- file.path(tmp_dir, "dwi.bvec")
  bval_file <- file.path(tmp_dir, "dwi.bval")

  write.dti.bvec(bvec_file, bvec)
  write.dti.bval(bval_file, bval)

  expect_equal(unname(read.dti.bvec(bvec_file)), bvec, tolerance = 1e-15)
  expect_equal(read.dti.bval(bval_file), bval, tolerance = 1e-15)
})


test_that("Negative zero is written as zero", {
  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bval_file <- file.path(tmp_dir, "dwi.bval")

  write.dti.bval(bval_file, c(0, -0, 1000))
  expect_false(any(grepl("-0", readLines(bval_file))))
  expect_equal(read.dti.bval(bval_file), c(0, 0, 1000))
})


test_that("Invalid arguments to the writers lead to errors", {
  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bvec_file <- file.path(tmp_dir, "dwi.bvec")
  bval_file <- file.path(tmp_dir, "dwi.bval")
  gt_file <- file.path(tmp_dir, "grad.b")

  # a table with neither 3 rows nor 3 columns is not a b-vector table
  expect_error(write.dti.bvec(bvec_file, matrix(1:8, nrow = 2)))
  expect_error(write.dti.bvec(bvec_file, "not a matrix"))
  expect_error(write.dti.bvec(bvec_file, rbind(c(1, 0, 0)), layout = "no_such_layout"))

  expect_error(write.dti.bval(bval_file, matrix(1:6, nrow = 2)))
  expect_error(write.dti.bval(bval_file, "abc"))
  expect_error(write.dti.bval(bval_file, c(0, 1000), layout = "no_such_layout"))

  # without b-values, the first argument must be a full gradient table
  expect_error(write.dti.grad(gt_file, rbind(c(1, 0, 0), c(0, 1, 0))))
  expect_error(write.dti.grad(gt_file, rbind(c(1, 0, 0), c(0, 1, 0)), c(0, 1000), layout = "no_such_layout"))
  # mismatching lengths are detected
  expect_error(write.dti.grad(gt_file, rbind(c(1, 0, 0), c(0, 1, 0)), c(0, 1000, 1000)))
})
