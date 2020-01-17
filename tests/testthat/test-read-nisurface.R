

test_that("A surface file in FreeSurfer binary format can be read using read_nisurfacefile", {

  fsbin_surface_file = system.file("extdata", "lh.tinysurface", package = "freesurferformats", mustWork = TRUE);
  surf = read_nisurfacefile(fsbin_surface_file);
  expect_true(is.fs.surface(surf));
  known_vertex_count = 5L;
  known_face_count = 3L;


  expect_equal(surf$mesh_face_type, "tris");

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(typeof(surf$faces), "integer");

  # Check whether vertex indices were incremented properly
  num_faces_with_index_zero = sum(surf$faces==0);
  expect_equal(num_faces_with_index_zero, 0);
})


test_that("A surface file in FreeSurfer ASCII format can be read using read_nisurfacefile", {

  fsasc_surface_file = system.file("extdata", "lh.tinysurface.asc", package = "freesurferformats", mustWork = TRUE);
  surf = read_nisurfacefile(fsasc_surface_file);
  expect_true(is.fs.surface(surf));
  known_vertex_count = 5L;
  known_face_count = 3L;


  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(typeof(surf$faces), "integer");

  # Check whether vertex indices were incremented properly
  num_faces_with_index_zero = sum(surf$faces==0);
  expect_equal(num_faces_with_index_zero, 0);
})


test_that("A surface files is discovered and read from its basename by read_nisurface", {

  fsasc_surface_file = system.file("extdata", "lh.testsurface.asc", package = "freesurferformats", mustWork = TRUE);
  path_including_basename = file.path(dirname(fsasc_surface_file), 'lh.testsurface');

  # Note: a file named like 'path_including_basename' does not exits. The read_nisurface function will check for
  # a number of surface file extensions (indlucing '.asc', the one that exists) and still discover and read the file:
  expect_false(file.exists(path_including_basename));
  surf = read_nisurface(path_including_basename);

  expect_true(is.fs.surface(surf));

  known_vertex_count = 5L;
  known_face_count = 3L;


  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(typeof(surf$faces), "integer");

  # Check whether vertex indices were incremented properly
  num_faces_with_index_zero = sum(surf$faces==0);
  expect_equal(num_faces_with_index_zero, 0);
})


