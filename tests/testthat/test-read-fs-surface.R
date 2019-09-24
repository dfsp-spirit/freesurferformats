test_that("Our demo surface file can be read using read.fs.surface", {

  surface_file = system.file("extdata", "lh.white.gz", package = "freesurferformats", mustWork = FALSE)
  skip_if_not(file.exists(surface_file), message="Test data missing.") # skip on travis

  surface_file = system.file("extdata", "lh.white.gz", package = "freesurferformats", mustWork = TRUE)
  surf = read.fs.surface(surface_file)
  known_vertex_count = 149244
  known_face_count = 298484


  expect_equal(surf$mesh_face_type, "tris");

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(typeof(surf$faces), "integer");
})

