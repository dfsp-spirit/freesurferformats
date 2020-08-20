
test_that("A Brainvoyager mesh can be written and re-read from a Brainvoyager SRF file.", {

  fsasc_surface_file = system.file("extdata", "lh.tinysurface.asc", package = "freesurferformats", mustWork = TRUE);
  orig_surf = read.fs.surface(fsasc_surface_file);


  bvsrf_file = tempfile(fileext = '.srf');
  write.fs.surface.bvsrf(bvsrf_file, orig_surf$vertices, orig_surf$faces);

  surf = read.fs.surface.bvsrf(bvsrf_file);
  expect_true(is.fs.surface(surf));

  known_vertex_count = 5L;
  known_face_count = 3L;

  # check orig surf
  expect_equal(nrow(orig_surf$vertices), known_vertex_count);
  expect_equal(nrow(orig_surf$faces), known_face_count);

  # now chech the BV surface
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


test_that("A bvsmp instance for writing Brainvoyager morph data can be created.", {
  data_length = 100L;
  morph_data = rnorm(data_length, 3.0, 1.0);
  bv = bvsmp(morph_data);
  expect_equal(bv$smp_version, 3L);
  expect_true(is.bvsmp(bv));
  expect_equal(bv$num_mesh_vertices, data_length);
  expect_equal(bv$num_maps, 1L);
  expect_equal(length(bv$vertex_maps[[1]]$data), data_length);
  expect_equal(bv$vertex_maps[[1]]$data, morph_data, tolerance = 1e-5);
})


test_that("Morphometry data can be written to and re-read from a Brainvoyager v3 SMP file.", {
  data_length = 100L;
  morph_data = rnorm(data_length, 3.0, 1.0);

  bvsmp_file = tempfile(fileext = '.smp');
  write.fs.morph.smp(bvsmp_file, morph_data, smp_version = 3L);

  bvsmp = read.smp.brainvoyager(bvsmp_file);
  expect_equal(bvsmp$smp_version, 3L);
  expect_equal(bvsmp$num_mesh_vertices, 100L);

  morph_data_reread = read.fs.morph(bvsmp_file);

  expect_equal(length(morph_data_reread), data_length);
  expect_equal(morph_data_reread, morph_data, tolerance = 1e-3);
})


test_that("Morphometry data can be written to and re-read from a Brainvoyager v2 SMP file.", {
  data_length = 100L;
  morph_data = rnorm(data_length, 3.0, 1.0);

  bvsmp_file = tempfile(fileext = '.smp');
  write.fs.morph.smp(bvsmp_file, morph_data, smp_version = 2L);

  bvsmp = read.smp.brainvoyager(bvsmp_file);
  expect_equal(bvsmp$smp_version, 2L);
  expect_equal(bvsmp$num_mesh_vertices, 100L);

  morph_data_reread = read.fs.morph(bvsmp_file);

  expect_equal(length(morph_data_reread), data_length);
  expect_equal(morph_data_reread, morph_data, tolerance = 1e-3);
})

