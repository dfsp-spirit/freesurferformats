test_that("One can write triangular surface data", {
  vertex_coords = matrix(rep(0.3, 15), nrow=3);
  faces = matrix(c(1L,2L,3L,2L,4L,3L,4L,5L,3L), nrow=3, byrow = TRUE);

  format_written = write.fs.surface(tempfile(fileext="white"), vertex_coords, faces);
  expect_equal(format_written, "tris");
})

test_that("One can write and re-read triangular surface data", {
  vertex_coords = matrix(rep(0.3, 15), nrow=5, ncol=3);
  faces = matrix(c(1L,2L,3L,2L,4L,3L,4L,5L,3L), nrow=3, ncol=3, byrow = TRUE);

  tmp_file = tempfile(fileext="white");
  format_written = write.fs.surface(tmp_file, vertex_coords, faces);
  expect_equal(format_written, "tris");

  cat(sprintf("Temp file written to temp file '%s'\n", tmp_file))

  surf = read.fs.surface(tmp_file);
  expect_equal(surf$internal$num_vertices_expected, 5)
  expect_equal(surf$internal$num_faces_expected, 3)
  expect_equal(nrow(surf$vertices), nrow(vertex_coords));
  expect_equal(nrow(surf$faces), nrow(faces));
  expect_equal(surf$mesh_face_type, "tris");

  expect_equal(surf$vertices, vertex_coords);
  expect_equal(surf$faces, faces);

})

