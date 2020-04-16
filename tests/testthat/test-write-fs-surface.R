
test_that("One can write triangular surface data", {
  vertex_coords = matrix(seq(1, 15)+0.5, nrow=3, byrow=TRUE);
  faces = matrix(c(1L,2L,3L,2L,4L,3L,4L,5L,3L), nrow=3, byrow = TRUE);

  format_written = write.fs.surface(tempfile(fileext="white"), vertex_coords, faces);
  expect_equal(format_written, "tris");
})

test_that("One can write and re-read triangular surface data", {
  vertex_coords = matrix(seq(1, 15)+0.5, nrow=5, ncol=3, byrow=TRUE);
  faces = matrix(c(1L,2L,3L,2L,4L,3L,4L,5L,3L), nrow=3, ncol=3, byrow = TRUE);

  tmp_file = tempfile(fileext="white");
  format_written = write.fs.surface(tmp_file, vertex_coords, faces);

  # Write a test file to a permanent location to manually check with freeview whether it gets read correctly ('freeview -f <file>').
  #format_written = write.fs.surface("/home/spirit/test.tiny.white", vertex_coords, faces);

  expect_equal(format_written, "tris");

  surf = read.fs.surface(tmp_file);
  expect_equal(surf$internal$num_vertices_expected, 5)
  expect_equal(surf$internal$num_faces_expected, 3)
  expect_equal(nrow(surf$vertices), nrow(vertex_coords));

  expect_equal(typeof(surf$faces), "integer");
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), nrow(faces));
  expect_equal(surf$mesh_face_type, "tris");

  expect_equal(surf$vertices, vertex_coords);
  expect_equal(surf$faces, faces);

})

test_that("One can read, write and re-read triangular surface data", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

  freesurferformats::download_optional_data();
  subjects_dir = freesurferformats::get_optional_data_filepath("subjects_dir");
  surface_file = file.path(subjects_dir, "subject1", "surf", "lh.white");

  skip_if_not(file.exists(surface_file), message="Test data missing.");

  surf = read.fs.surface(surface_file);

  tmp_file = tempfile(fileext="white");
  format_written = write.fs.surface(tmp_file, surf$vertices, surf$faces);

  # One should also write the file to some permament location and manually ensure that freeview will open it correctly ('freeview -f <file>'):
  #format_written = write.fs.surface("/home/spirit/test.lh.white", surf$vertices, surf$faces);

  expect_equal(format_written, "tris");

  surf_re = read.fs.surface(tmp_file);
  expect_equal(surf$internal$num_vertices_expected, surf_re$internal$num_vertices_expected);
  expect_equal(surf$internal$num_faces_expected, surf_re$internal$num_faces_expected);
  expect_equal(nrow(surf$vertices), nrow(surf_re$vertices));
  expect_equal(nrow(surf$faces), nrow(surf_re$faces));
  expect_equal(surf$mesh_face_type, "tris");
  expect_equal(surf$mesh_face_type, surf_re$mesh_face_type);

  expect_equal(surf$vertices, surf_re$vertices);
  expect_equal(surf$faces, surf_re$faces);
})


test_that("Surface files in VTK format can be read and written", {

  surface_file = system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file);

  tmp_vtk_file = tempfile(fileext=".vtk");
  write.fs.surface.vtk(tmp_vtk_file, surf$vertices, surf$faces);

  surf_re = read.fs.surface(tmp_vtk_file);
  expect_equal(surf$vertices, surf_re$vertices);
  expect_equal(surf$faces, surf_re$faces);
})

