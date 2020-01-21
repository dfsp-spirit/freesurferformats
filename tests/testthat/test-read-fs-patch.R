

test_that("A FreeSurfer binary patch file can be read using read.fs.patch", {

  skip("This test has to be run manually, it requires a FreeSurfer installation with env var FREESURFER_HOME set correctly.");

  #fsaverage_dir = file.path(Sys.getenv('HOME'), 'software', 'freesurfer', 'subjects', 'fsaverage');
  fsaverage_dir = file.path(Sys.getenv('FREESURFER_HOME'), 'subjects', 'fsaverage');
  patch_file = file.path(fsaverage_dir, 'surf', 'lh.cortex.patch.3d');

  fspatch = read.fs.patch(patch_file);
  patch_data = fspatch$vertices;

  expect_true(is.matrix(patch_data));
  expect_equal(ncol(patch_data), 7);
  expect_equal(nrow(patch_data), 149297);

  # Test indices:
  expect_equal(min(patch_data[,1]), 1);   # smallest one-based index must be 1
  expect_equal(min(patch_data[,7]), 0);   # smallest zero-based index must be 0
})


# There currently is not test for the ASCII version (read.fs.patch.asc), as we do not have an example file.


