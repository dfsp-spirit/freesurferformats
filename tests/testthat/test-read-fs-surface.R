

test_that("Our demo surface file can be read using read.fs.surface", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  surface_file = file.path(subjects_dir, "subject1", "surf", "lh.white");
  skip_if_not(file.exists(surface_file), message="Test data missing.") ;

  surf = read.fs.surface(surface_file);
  expect_true(is.fs.surface(surf));
  known_vertex_count = 149244;
  known_face_count = 298484;


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


test_that("Vertex connectivity in the demo surface file is as expected from reference implementation", {
  # Checks for bug in issue #8: freesurfer surface import - row-major/column-major order
  # The vertex connectivities used in this test are known from running the FreeSurfer Matlab function $FREESURFER_HOME/matlab/read_surf.m on the data

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  surface_file = file.path(subjects_dir, "subject1", "surf", "lh.white");
  skip_if_not(file.exists(surface_file), message="Test data missing.");

  surf = read.fs.surface(surface_file);
  expect_true(is.fs.surface(surf));

  # This test assures that the vertices are connected as defined in the reference implementation.
  expect_equal(surf$faces[1,], c(0, 1, 5) + 1);     # the +1 is because R using 1-based indexing
  expect_equal(surf$faces[100,], c(137, 130, 41) + 1);
  expect_equal(surf$faces[1000,], c(770, 444, 757) + 1);

  expect_equal(surf$vertices[1,], c(-1.8522, -107.9827, 22.7697), tolerance=1e-2);
  expect_equal(surf$vertices[100,], c(-4.3814, -107.6495, 24.0106), tolerance=1e-2);
  expect_equal(surf$vertices[1000,], c(-5.2788, -103.5765, 19.6616), tolerance=1e-2);
})


test_that("The vertices of a face are close to each other", {
  # Checks for bug in issue #8: freesurfer surface import - row-major/column-major order

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");

  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  surface_file = file.path(subjects_dir, "subject1", "surf", "lh.white");
  skip_if_not(file.exists(surface_file), message="Test data missing.");

  surf = read.fs.surface(surface_file);

  expect_true(is.fs.surface(surf));

  # Test that the distance between the vertices of a face is small. For brain surface meshes, the
  # coords are given in mm and the resolution is quite high. It is definitely sane to request that
  # the vertices should not be more than 2 mm apart:
  test_faces = c(1, 50, 100, 1000, 10000, 42);
  for(face_idx in test_faces) {
    vertex_indices_of_face = surf$faces[face_idx,];
    vertex_coords_of_face = surf$vertices[vertex_indices_of_face,];
    dist_matrix_face_verts = stats::dist(vertex_coords_of_face);
    expect_true(max(dist_matrix_face_verts) < 2.0);
  }

})


test_that("The lh.white of Bert can be read using read.fs.surface", {

  surface_file = system.file("extdata", "bert.lh.white", package = "freesurferformats", mustWork = FALSE)
  skip_if_not(file.exists(surface_file), message="Test data missing.");

  surface_file = system.file("extdata", "bert.lh.white", package = "freesurferformats", mustWork = TRUE)
  surf = read.fs.surface(surface_file);
  expect_true(is.fs.surface(surf));
  known_vertex_count_bert = 133176;
  known_face_count_bert = 266348;


  expect_equal(surf$mesh_face_type, "tris");

  expect_true(is.fs.surface(surf));
  expect_false(is.fs.surface(list("whatever"=6)));

  expect_equal(nrow(surf$vertices), known_vertex_count_bert);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count_bert);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(typeof(surf$faces), "integer");

  # Check whether vertex indices were incremented properly
  num_faces_with_index_zero = sum(surf$faces==0);
  expect_equal(num_faces_with_index_zero, 0);

  # This test assures that the vertices are connected as defined in the reference implementation.
  expect_equal(surf$faces[1,], c(0, 1, 3) + 1);     # the +1 is because R using 1-based indexing
  expect_equal(surf$faces[100,], c(140, 36, 139) + 1);
  expect_equal(surf$faces[1000,], c(440, 454, 441) + 1);

  expect_equal(surf$vertices[1,], c(-12.6998, -102.2399, -10.0076), tolerance=1e-2);
  expect_equal(surf$vertices[100,], c(-12.234, -100.672, 0.129), tolerance=1e-2);
  expect_equal(surf$vertices[1000,], c(-18.8, -97.6, -16.8), tolerance=1e-2);

  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("A surface file in FreeSurfer ASCII format can be read using read.fs.surface", {

  fsasc_surface_file = system.file("extdata", "lh.tinysurface.asc", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(fsasc_surface_file);
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
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("A surface file in STL binary format can be read using read.fs.surface", {
  stlbin_surface_file = system.file("extdata", "cube_bin.stl", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(stlbin_surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("A surface file in BYU ASCII mesh format can be read using read.fs.surface", {
  byu_surface_file = system.file("extdata", "cube_quads.byu", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(byu_surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle

  # The file actually contains quadrangular polygons, not triangles. The function
  # remeshes automatically, but we can access the original quads as well:
  expect_equal(ncol(surf$metadata$faces_quads), 4);      # the 4 vertex indices of a quad
  expect_equal(nrow(surf$metadata$faces_quads), known_face_count / 2L);
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("Surface files in GEO format can be read using read.fs.surface", {
  surface_file = system.file("extdata", "cube.geo", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("Surface files in STL format can be read using read.fs.surface", {
  surface_file = system.file("extdata", "cube.stl", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("Surface files in TRI format can be read using read.fs.surface", {
  surface_file = system.file("extdata", "cube.tri", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("Surface files in VTK format can be read using read.fs.surface", {
  surface_file = system.file("extdata", "cube.vtk", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file);

  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


test_that("Surface files in Wavefront OBJ format can be read using read.fs.surface", {

  # I had to change the file extension of demo .obj files that come with this package to
  # '.wobj', because the file extension '.obj' triggered false positive warnings on CRAN. ~
  surface_file = system.file("extdata", "cube.wobj", package = "freesurferformats", mustWork = TRUE);
  surf = read.fs.surface(surface_file, format = 'obj');



  known_vertex_count = 8L;
  known_face_count = 12L;

  expect_equal(nrow(surf$vertices), known_vertex_count);
  expect_equal(ncol(surf$vertices), 3);      # the 3 coords (x,y,z)
  expect_equal(typeof(surf$vertices), "double");

  expect_equal(nrow(surf$faces), known_face_count);
  expect_equal(ncol(surf$faces), 3);      # the 3 vertex indices of a triangle
  expect_equal(min(surf$faces), 1L);  # vertex indices must start at 1
})


