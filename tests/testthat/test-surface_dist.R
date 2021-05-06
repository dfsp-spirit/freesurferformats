
test_that("We can compute the vertex closest to an MNI152 coordinate on the fsaverage surface.", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");

  # WARNING: This surface is NOT the fsaverage surface, as we do not ship this surface with
  # freesurferformats. You have to replace this with the fsaverage surface for this demo to
  # do what you expect, e.g., if you have FreeSurfer installed and setup correctly:
  #surface_file_lh = file.path(Sys.getenv("FREESURFER_HOME"), "subjects", "fsaverage", "surf", "lh.white");
  #surface_file_rh = file.path(Sys.getenv("FREESURFER_HOME"), "subjects", "fsaverage", "surf", "rh.white");
  # or
  #surface_file_lh = file.path("~/software/freesurfer/", "subjects", "fsaverage", "surf", "lh.white");
  #surface_file_rh = file.path("~/software/freesurfer/", "subjects", "fsaverage", "surf", "rh.white");
  #
  # Also, keep in mind that the point could be on the other hemisphere and handle that.
  surface_file_lh = file.path(subjects_dir, "subject1", "surf", "lh.white");
  surface_file_rh = file.path(subjects_dir, "subject1", "surf", "rh.white");

  skip_if_not(file.exists(surface_file_lh), message="Test data missing.");
  skip_if_not(file.exists(surface_file_rh), message="Test data missing.");

  surf_lh = read.fs.surface(surface_file_lh);
  surf_rh = read.fs.surface(surface_file_rh);
  mni_152_point = c(-47.0, 46.0, 22.0);  # see the point at http://human.brain-map.org/mri_viewers/data
  surface_space_point = doapply.transform.mtx(mni_152_point, solve(mni152reg()));
  surface_vertex_lh = closest.vert.to.point(surf_lh, surface_space_point);
  surface_vertex_rh = closest.vert.to.point(surf_rh, surface_space_point);

  #testthat::expect_equal(surface_vertex_lh$vertex_id[1], 93970);
  #testthat::expect_equal(surface_vertex_lh$dist[1], 10.85414, tolerance = 1e-4);
})

