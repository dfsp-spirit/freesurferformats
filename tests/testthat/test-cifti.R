
test_that("We can load a CIFTI dscalar file as a morph data vector", {

  # The files used in this test are provided under the Open Data Commons Public Domain Dedication and Licence (PDDL), see http://opendatacommons.org/licenses/pddl/1.0/ for details.
  # These are the CIFTI 2 example files available on https://www.nitrc.org/projects/cifti/

  testthat::skip_on_cran(); # cannot download testdata on CRAN.
  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  cifti_dir = freesurferformats::get_opt_data_filepath("cifti");
  cii_file = file.path(cifti_dir, 'Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii');
  skip_if_not(file.exists(cii_file), message="Test data missing.") ;

  # We need to read the cifti file in the client code until the fix for bug #9 in the cifti
  # package (see https://github.com/muschellij2/cifti/issues/9) is on CRAN. As of 2020-08-11 (cifti v0.4.5),
  # it is only fixed in the development version on GitHub. If you install cifti from github using devtools::install_github, you
  # should be fine, however we cannot expect all users to do this, so this test shows how to work around the issue with
  # the CRAN version of 'cifti'.

  # morph_lh = read.fs.morph.cifti(cii_file, 'lh'); # Myelin data read directly from file: this does not work atm, see above.

  if (requireNamespace("cifti", quietly = TRUE)) {
    library('cifti');
    cii = cifti::read_cifti(cii_file);   # read in client code, with cifti package loaded.
    morph_lh = read.fs.morph.cifti(cii, 'lh'); # Myelin data
    morph_rh = read.fs.morph.cifti(cii, 'rh');
    morph2_lh = read.fs.morph.cifti(cii, 'lh', 2L); # Cortical Thickness data
    morph2_rh = read.fs.morph.cifti(cii, 'rh', 2L);
    morph2_both = read.fs.morph.cifti(cii, 'both', 2L);

    # read by structure index
    morph2_lh_again = read.fs.morph.cifti(cii, 1L, 2L);

    sf_lh = freesurferformats::read.fs.surface(file.path(cifti_dir, "Conte69.L.inflated.32k_fs_LR.surf.gii"));
    sf_rh = freesurferformats::read.fs.surface(file.path(cifti_dir, "Conte69.R.inflated.32k_fs_LR.surf.gii"));

    expect_equal(length(morph_lh), nrow(sf_lh$vertices));
    expect_equal(length(morph2_lh), nrow(sf_lh$vertices));
    expect_equal(length(morph_rh), nrow(sf_rh$vertices));
    expect_equal(length(morph2_rh), nrow(sf_rh$vertices));
    expect_equal(length(morph2_both), (nrow(sf_lh$vertices)+nrow(sf_rh$vertices)));

    # Expect error on invalid structure
    expect_error(read.fs.morph.cifti(cii, 'no_such_structure', 2L)); # invalid brain structure.
  }
  # You could visualize the data on the surface in fsbrain like this:
  # fsbrain::vis.fs.surface(list('lh'=sf_lh, 'rh'=sf_rh), per_vertex_data = list('lh'=morph2_lh, 'rh'=morph2_rh));
})
