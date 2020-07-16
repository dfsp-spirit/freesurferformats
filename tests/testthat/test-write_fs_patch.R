

test_that("A tiny patch can be written to binary patch format and re-read", {

  # create a patch
  num_vertices = 6L;   # a tiny patch
  vertices = matrix(rep(0., num_vertices*5), ncol=5);
  vertices[,1] = seq.int(num_vertices);  # 1-based vertex indices
  vertices[,2:4] = matrix(rnorm(num_vertices*3, 8, 2), ncol=3);  # vertex coords
  vertices[,5] = rep(0L, num_vertices);  # is_border
  vertices[3,5] = 1L;  # set a vertex to be a border vertex
  patch = fs.patch(vertices);

  patch_file = tempfile(fileext = ".patch");
  write.fs.patch(patch_file, patch);

  same_patch = read.fs.patch(patch_file);
  expect_equal(nrow(patch$vertices), nrow(same_patch$vertices));
})
