
test_that("We can compute the vertex closest to an MNI152 coordinate on the fsaverage surface.", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");

  # WARNING: This surface is NOT the fsaverage surface, as we do not ship this surface with
  # freesurferformats. You have to replace this with the fsaverage surface for this demo to
  # do what you expect, e.g., if you have FreeSurfer installed and setup correctly:
  #surface_file = file.path(Sys.getenv("FREESURFER_HOME"), "subjects", "fsaverage", "surf", "lh.white");
  # Also, keep in mind that the point could be on the other hemisphere and handle that.
  surface_file = file.path(subjects_dir, "subject1", "surf", "lh.white");

  skip_if_not(file.exists(surface_file), message="Test data missing.") ;

  surf = read.fs.surface(surface_file);
  mni_152_point = c(1.0, 1.0, 1.0);
  surface_space_point = doapply.transform.mtx(mni_152_point, mni152reg());
  surface_vertex = closest.vert.to.point(surf, surface_space_point);

  testthat::expect_equal(surface_vertex$vertex_id[1], 98397);
  testthat::expect_equal(surface_vertex$dist[1], 39.3773, tolerance = 1e-4);
})

