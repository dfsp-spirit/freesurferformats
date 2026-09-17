# Tests for reading diffusion MRI gradient tables: FSL bvecs/bvals pairs and
# MRtrix gradient tables. The layout rules implemented here were cross-checked
# against MRtrix3 (core/dwi/gradient.cpp, load_bvecs_bvals()) and DIPY
# (dipy/io/gradients.py, read_bvals_bvecs()); see R/read_dwi_gradients.R for the
# references. Note that nibabel has no reader for these files at all (only
# Philips PAR/REC headers carry gradient information there), so DIPY is the
# Python reference for this format.


test_that("One can read a FSL-style bvec/bval pair in the standard layout", {
  files <- write_test_gradient_pair(
    bvec_lines = c("1 0 0 0", "0 1 0 0", "0 0 1 0"),
    bval_lines = "0 1000 2000 3000"
  )

  bvec <- read.dti.bvec(files$bvec)
  expect_equal(dim(bvec), c(4L, 3L))
  expect_equal(unname(bvec[, 1]), c(1, 0, 0, 0))
  expect_equal(unname(bvec[, 2]), c(0, 1, 0, 0))
  expect_equal(unname(bvec[, 3]), c(0, 0, 1, 0))
  expect_equal(unname(bvec[2, ]), c(0, 1, 0))

  bval <- read.dti.bval(files$bval)
  expect_equal(bval, c(0, 1000, 2000, 3000))
})


test_that("A bval file with one value per line is read", {
  # QSIPrep writes the b-values this way, and the HCP distributes them like this.
  bval_file <- write_test_gradient_file(c("0", "1000", "1000", "2000"), name = "bvals")
  expect_equal(read.dti.bval(bval_file), c(0, 1000, 1000, 2000))
})


test_that("A bvec file with one volume per line is read", {
  bvec_file <- write_test_gradient_file(c("1 0 0", "0 1 0", "0 0 1", "0.5 0.5 0"), name = "bvecs")
  expect_silent(bvec <- read.dti.bvec(bvec_file))
  expect_equal(dim(bvec), c(4L, 3L))
  expect_equal(unname(bvec[1, ]), c(1, 0, 0))
  expect_equal(unname(bvec[4, ]), c(0.5, 0.5, 0))
})


test_that("A square bvec file is ambiguous: it warns and reads the lines as components", {
  bvec_file <- write_test_gradient_file(c("1 2 3", "4 5 6", "7 8 9"), name = "ambiguous.bvec")

  # The lines are the components, which is what MRtrix3 does with such a file.
  expect_warning(bvec <- read.dti.bvec(bvec_file))
  expect_equal(dim(bvec), c(3L, 3L))
  expect_equal(unname(bvec[1, ]), c(1, 4, 7))
  expect_equal(unname(bvec[3, ]), c(3, 6, 9))

  # The other interpretation, which is what DIPY reads, is available explicitly.
  expect_silent(bvec_volumes <- read.dti.bvec(bvec_file, layout = "volumes"))
  expect_equal(unname(bvec_volumes[1, ]), c(1, 2, 3))
  expect_equal(unname(bvec_volumes[3, ]), c(7, 8, 9))

  expect_silent(bvec_components <- read.dti.bvec(bvec_file, layout = "components"))
  expect_equal(unname(bvec_components[1, ]), c(1, 4, 7))
})


test_that("The explicit layouts are checked against the file", {
  volumes_layout_file <- write_test_gradient_file(c("1 0 0", "0 1 0", "0 0 1", "1 1 1"), name = "v.bvec")
  expect_error(read.dti.bvec(volumes_layout_file, layout = "components"))

  components_layout_file <- write_test_gradient_file(c("1 0 0 0", "0 1 0 0", "0 0 1 1"), name = "c.bvec")
  expect_error(read.dti.bvec(components_layout_file, layout = "volumes"))
})


test_that("Blank lines, comments, commas and tabs are handled", {
  bvec_file <- write_test_gradient_file(c("# a comment", "", "1,0,0,0", "0\t1\t0\t0", "  0 0 1 0  "), name = "messy.bvec")
  expect_silent(bvec <- read.dti.bvec(bvec_file))
  expect_equal(dim(bvec), c(4L, 3L))
  expect_equal(unname(bvec[2, ]), c(0, 1, 0))
})


test_that("Gzip-compressed gradient files are detected by their magic bytes", {
  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  gz_path <- file.path(tmp_dir, "dwi.bval.gz")
  write.dti.bval(gz_path, c(0, 1000, 1000))
  expect_equal(read.dti.bval(gz_path), c(0, 1000, 1000))

  # The same content under a name without a .gz extension must still be read.
  renamed_path <- file.path(tmp_dir, "renamed.bval")
  file.copy(gz_path, renamed_path)
  expect_equal(read.dti.bval(renamed_path), c(0, 1000, 1000))
})


test_that("Invalid gradient files lead to errors", {
  bvec_file <- write_test_gradient_file(c("1 0 0 0", "0 1 0 0", "0 0 1 0"), name = "dwi.bvec")
  expect_error(read.dti.bvec(bvec_file, layout = "no_such_layout"))
  expect_error(read.dti.bval(bvec_file, layout = "no_such_layout"))

  expect_error(read.dti.bvec(file.path(tempdir(), "no_such_file_404.bvec")))
  expect_error(read.dti.bvec(tempdir()))

  # a table that has neither 3 rows nor 3 columns
  expect_error(read.dti.bvec(write_test_gradient_file(c("1 0", "0 1", "0 0", "1 1", "2 2"), name = "bad.bvec")))
  # non-numeric content
  expect_error(read.dti.bvec(write_test_gradient_file(c("a b c", "1 2 3", "4 5 6"), name = "notnum.bvec")))
  # lines with different numbers of values
  expect_error(read.dti.bvec(write_test_gradient_file(c("1 0 0", "0 1"), name = "ragged.bvec")))
  # an empty file
  expect_error(read.dti.bvec(write_test_gradient_file(character(0), name = "empty.bvec")))
  # a bval table with more than one row and more than one column
  expect_error(read.dti.bval(write_test_gradient_file(c("0 1000", "1000 2000"), name = "bad.bval")))
})


test_that("One can read a gradient table in MRtrix format", {
  gt_file <- write_test_gradient_file(c("0 0 0 0", "1 0 0 1000", "0 1 0 1000", "-0.5 0.5 0.7071067 2000"), name = "grad.b")

  expect_silent(gt <- read.dti.grad(gt_file))
  expect_equal(dim(gt), c(4L, 4L))
  expect_equal(colnames(gt), c("x", "y", "z", "b"))
  expect_equal(unname(gt[, 4]), c(0, 1000, 1000, 2000))
  expect_equal(unname(gt[4, 1]), -0.5)
})


test_that("A gradient table with 4 volumes is not an ambiguous square", {
  gt_file <- write_test_gradient_file(c("1 0 0 1000", "0 1 0 1000", "0 0 1 1000", "1 1 0 2000"), name = "grad4.b")
  expect_silent(gt <- read.dti.grad(gt_file))
  expect_equal(dim(gt), c(4L, 4L))
  # the rows are the volumes, not the components
  expect_equal(unname(gt[1, 1:3]), c(1, 0, 0))
  expect_equal(unname(gt[4, 1:3]), c(1, 1, 0))
})


test_that("A transposed gradient table and a count header are accepted", {
  transposed <- write_test_gradient_file(c("1 0 0", "0 1 0", "0 0 1", "1000 1000 1000"), name = "grad_t.b")
  expect_silent(gt <- read.dti.grad(transposed))
  expect_equal(dim(gt), c(3L, 4L))
  expect_equal(unname(gt[, 4]), c(1000, 1000, 1000))
  expect_equal(unname(gt[, 1]), c(1, 0, 0))

  counted <- write_test_gradient_file(c("3", "0 0 0 0", "1 0 0 1000", "0 1 0 1000"), name = "grad_c.b")
  expect_warning(gt2 <- read.dti.grad(counted))
  expect_equal(dim(gt2), c(3L, 4L))
  expect_equal(unname(gt2[, 4]), c(0, 1000, 1000))
})


test_that("read.dti.gradients reads a pair of files or in-memory data", {
  files <- write_test_gradient_pair(c("0 1 0 0", "0 0 1 0", "0 0 0 1"), "0 1000 1000 2000")

  expect_silent(grad <- read.dti.gradients(files$bvec, files$bval))
  expect_equal(names(grad), c("bvec", "bval"))
  expect_equal(dim(grad$bvec), c(4L, 3L))
  expect_equal(grad$bval, c(0, 1000, 1000, 2000))

  # in-memory data works as well, and the result is the same
  expect_silent(grad2 <- read.dti.gradients(grad$bvec, grad$bval))
  expect_equal(grad2$bvec, grad$bvec)
  expect_equal(grad2$bval, grad$bval)

  # the number of volumes of the image can be checked
  expect_silent(read.dti.gradients(files$bvec, files$bval, n_volumes = 4L))
  expect_error(read.dti.gradients(files$bvec, files$bval, n_volumes = 5L))

  # mismatching lengths are an error
  expect_error(read.dti.gradients(files$bvec, c(0, 1000)))
  expect_error(read.dti.gradients(rbind(c(1, 0, 0), c(0, 1, 0)), c(0, 1000, 1000)))
})


test_that("read.dti.gradients finds the bval file or a gradient table file", {
  files <- write_test_gradient_pair(c("0 1 0 0", "0 0 1 0", "0 0 0 1"), "0 1000 1000 2000")

  # the b-values file is found by name next to the b-vectors file
  expect_silent(grad <- read.dti.gradients(files$bvec))
  expect_equal(grad$bval, c(0, 1000, 1000, 2000))
  expect_equal(dim(grad$bvec), c(4L, 3L))

  # a single file with 4 values per volume is read as an MRtrix gradient table
  gt_file <- write_test_gradient_file(c("0 0 0 0", "1 0 0 1000", "0 1 0 1000"), name = "grad.b")
  expect_silent(grad2 <- read.dti.gradients(gt_file))
  expect_equal(dim(grad2$bvec), c(3L, 3L))
  expect_equal(grad2$bval, c(0, 1000, 1000))

  # a file that is neither, and has no b-values file next to it, is an error
  lonely <- write_test_gradient_file(c("1 0", "0 1", "0 0"), name = "lonely.bvec")
  expect_error(read.dti.gradients(lonely))
})


test_that("Missing values are read as b=0 volumes, or reported as corrupt", {
  bvec <- rbind(c(0, 0, 0), c(0, 0, 0), c(0, 0, 1))

  # a missing b-value in a volume without a direction is a b=0 volume
  expect_warning(grad <- validate.dti.gradients(bvec, c(0, NaN, 1000)))
  expect_equal(grad$bval, c(0, 0, 1000))
  expect_equal(unname(grad$bvec[2, ]), c(0, 0, 0))

  # a missing b-value in a volume with a direction is corrupt
  expect_error(validate.dti.gradients(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 0, 1)), c(0, NaN, 1000)))

  # a missing direction in a volume with a non-zero b-value is corrupt
  expect_error(validate.dti.gradients(rbind(c(0, 0, 0), c(NA, NA, NA), c(0, 0, 1)), c(0, 1000, 1000)))

  # missing both is a b=0 volume
  expect_warning(grad2 <- validate.dti.gradients(rbind(c(0, 0, 0), c(NA, NA, NA), c(0, 0, 1)), c(0, NA, 1000)))
  expect_equal(grad2$bval, c(0, 0, 1000))
})


test_that("Suspicious but readable gradient tables lead to warnings, not to silent edits", {
  # a gradient vector that is not a unit vector: MRtrix3 would rescale the b-value
  expect_warning(grad <- validate.dti.gradients(rbind(c(0, 0, 0), c(2, 0, 0), c(0, 0, 1)), c(0, 1000, 1000)))
  expect_equal(unname(grad$bvec[2, ]), c(2, 0, 0))
  expect_equal(grad$bval, c(0, 1000, 1000))

  # a b-value above the b=0 threshold but no direction
  expect_warning(validate.dti.gradients(rbind(c(0, 0, 0), c(0, 0, 0), c(0, 0, 1)), c(0, 3000, 1000)))

  # a direction but a b-value below the b=0 threshold
  expect_warning(validate.dti.gradients(rbind(c(0, 0, 0), c(1, 0, 0), c(0, 0, 1)), c(0, 5, 1000)))
})


test_that("Invalid arguments to validate.dti.gradients lead to errors", {
  two_volumes <- rbind(c(1, 0, 0), c(0, 1, 0))

  expect_error(validate.dti.gradients("not a matrix", c(0, 1000)))
  expect_error(validate.dti.gradients(two_volumes, c(0, 1000, 1000)))
  expect_error(validate.dti.gradients(two_volumes, c("a", "b")))
  expect_error(validate.dti.gradients(two_volumes, matrix(1:6, nrow = 2)))
  expect_error(validate.dti.gradients(two_volumes, c(0, Inf)))
  expect_error(validate.dti.gradients(two_volumes, c(0, 1000), n_volumes = "many"))
  expect_error(validate.dti.gradients(two_volumes, c(0, 1000), n_volumes = 7L))
})


test_that("A real QSIPrep gradient pair is read correctly", {
  bvec_file <- find_extra_test_data_file(file.path("dwi", "sub-01_space-ACPC_desc-preproc_dwi.bvec"))
  bval_file <- find_extra_test_data_file(file.path("dwi", "sub-01_space-ACPC_desc-preproc_dwi.bval"))
  testthat::skip_if(is.null(bvec_file) || is.null(bval_file), message = "extra_test_data not available in this environment (a git checkout of the repository is required).")

  # This file has 72 volumes in a single shell, with 8 b=0 volumes. Note that
  # QSIPrep writes the b-values as one value per line and the b-vectors in the
  # three-line FSL layout, which is exactly the combination that naive readers
  # get wrong.
  expect_silent(grad <- read.dti.gradients(bvec_file, bval_file, n_volumes = 72L))
  expect_equal(dim(grad$bvec), c(72L, 3L))
  expect_equal(length(grad$bval), 72L)
  expect_equal(sum(grad$bval == 0), 8L)
  expect_equal(sum(grad$bval == 1000), 64L)
  expect_equal(length(readLines(bval_file)), 72L)
  expect_equal(length(readLines(bvec_file)), 3L)

  # all diffusion-weighted gradients are unit vectors
  norms <- sqrt(rowSums(grad$bvec^2))
  expect_true(all(abs(norms[grad$bval > 10] - 1) < 1e-4))

  # the b-values file is found automatically next to the b-vectors file
  expect_silent(grad2 <- read.dti.gradients(bvec_file, n_volumes = 72L))
  expect_equal(grad2$bvec, grad$bvec)
  expect_equal(grad2$bval, grad$bval)
})
