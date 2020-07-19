
# Unit tests testing the functions in helpers.R.

test_that("The readable.files function works as expected.", {
  subdir = sprintf('dir_%d', sample.int(10000L, 1L));
  empty_td = file.path(tempdir(), subdir);
  dir.create(empty_td);
  file.create(file.path(empty_td, 'lh.area'));
  file.create(file.path(empty_td, 'lh.area.mgh'));
  file.create(file.path(empty_td, 'lh.area.mgz'));

  expect_equal(length(readable.files(file.path(empty_td, 'lh.area'))), 1L); # 2 found, only 1 returned
  expect_equal(length(readable.files(file.path(empty_td, 'lh.area'), return_all = TRUE)), 2L); # 2 found, 2 returned

  expect_equal(length(readable.files(file.path(empty_td, 'lh.nosuchfile'), error_if_none = FALSE)), 0L); # 0 found, okay
  expect_error(readable.files(file.path(empty_td, 'lh.nosuchfile'), error_if_none = TRUE)); # 0 found, not okay

})
