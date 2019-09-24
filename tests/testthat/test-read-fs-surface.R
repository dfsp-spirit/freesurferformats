test_that("Our demo surface file can be read using read.fs.surface", {
  surface_file = system.file("extdata", "lh.white.gz", package = "freesurferformats", mustWork = TRUE)
  surf = read.fs.surface(surface_file)
  known_vertex_count = 149244
  known_face_count = 298484


  expect_equal(surf$mesh_face_type, "tris");

  expect_equal(length(surf$vertices), known_vertex_count);
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(length(surf$faces), known_face_count);
  expect_equal(typeof(surf$faces), "integer");
})

