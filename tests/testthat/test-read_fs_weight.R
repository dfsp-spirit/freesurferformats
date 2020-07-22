# More tests are in test-write_fs_weight

test_that("Invalid arguments to read.fs.weight lead to errors", {
  tmp_file = tempfile(fileext=".w");

  expect_error(read.fs.weight(tmp_file, format = "no such format")); # invalid format
})

