#' @title Download optional data for the freesurferformats package.
#'
#' @description Ensure that the optional data is available locally in the package cache. Will try to download the data only if it is not available. This data is not required for the package to work, but it is used in the examples, in the unit tests and also in the example code from the vignette. Downloading it is highly recommended.
#'
#' @return Named list. The list has entries: "available": vector of strings. The names of the files that are available in the local file cache. You can access them using get_optional_data_file(). "missing": vector of strings. The names of the files that this function was unable to retrieve.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info ensure_files_available
download_opt_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("freesurferformats");

  # Replace these with your optional data files.
  base_path_subject1 = c('subjects_dir', 'subject1');
  local_filenames_subject1 = list(c(base_path_subject1, 'label', 'lh.aparc.a2009s.annot'),
                                  c(base_path_subject1, 'label', 'lh.aparc.annot'),
                                  c(base_path_subject1, 'label', 'lh.cortex.label'),
                                  c(base_path_subject1, 'label', 'rh.aparc.a2009s.annot'),
                                  c(base_path_subject1, 'label', 'rh.aparc.annot'),
                                  c(base_path_subject1, 'label', 'rh.cortex.label'),
                                  c(base_path_subject1, 'mri', 'brain.mgz'),
                                  c(base_path_subject1, 'surf', 'lh.thickness'),
                                  c(base_path_subject1, 'surf', 'lh.white'),
                                  c(base_path_subject1, 'surf', 'rh.thickness'),
                                  c(base_path_subject1, 'surf', 'rh.white'),
                                  c(base_path_subject1, 'surf', 'lh.thickness.fwhm10.fsaverage.mgh'),
                                  c(base_path_subject1, 'surf', 'rh.thickness.fwhm10.fsaverage.mgh'),
                                  c(base_path_subject1, 'surf', 'lh.area'),
                                  c(base_path_subject1, 'surf', 'rh.area'),
                                  c(base_path_subject1, 'surf', 'lh.area.fwhm10.fsaverage.mgh'),
                                  c(base_path_subject1, 'surf', 'rh.area.fwhm10.fsaverage.mgh'),
                                  c(base_path_subject1, 'label', 'lh.aparc.a2005s.annot'),
                                  c(base_path_subject1, 'label', 'rh.aparc.a2005s.annot'),
                                  c(base_path_subject1, 'ext', 'lh.cortex.patch.3d'),
                                  c(base_path_subject1, 'ext', 'lh.cortex.patch.3d.asc'),
                                  c(base_path_subject1, 'surf', 'lh.thickness.nii.gz')

  );

  base_path_cifti = c('cifti');
  local_filenames_cifti = list(c(base_path_cifti, 'Conte69.L.inflated.32k_fs_LR.surf.gii'),
                               c(base_path_cifti, 'Conte69.R.inflated.32k_fs_LR.surf.gii'),
                               c(base_path_cifti, 'Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii')
                          );


  base_path_nifti2 = c('nifti2');
  local_filenames_nifti2 = list(c(base_path_nifti2, 'avg152T1_LR_nifti2.nii.gz'));

  local_filenames = c(local_filenames_subject1, local_filenames_cifti, local_filenames_nifti2);


  md5sums_subject1 = c('099e738654aedc71cd580256f4f3914b',
                       'c0ac6e39e3536ef968f1051f908a80c4',
                       '929873a4ae3542331d84e5a97c852824',
                       '31dd188591d11784f9efed66601b9267',
                       'd18d466b35f64ec5ff1c97d885095863',
                       '6b650fa9076561d96cd4ac1bbb6dd55d',
                       '2c4874576eb935bf9445dda0529774e0',
                       '96d6350a6b158453a0231a1f01cfbd58',
                       'b6d2cdb9793aae3b76c2dcbf03491988',
                       '4ec315e8daa6c3bbda46c36b9188b60f',
                       '8034395bc9fcfa02c05c6cf6559ab97e',
                       'c58c2dcfc093f409cc1d6a431ac676de',
                       '2ef44d7a40590242c2516930443712e4',
                       '33ac2ccf1cd388e458e2d03fcc5cc3e6',
                       '1e55e15bb468652b5b024daf37c4ec12',
                       '492516bb4e1e31224dba96b2da0d07c4',
                       '4b7d88484182326576218abdce8ac378',
                       '718c140022bf14a33b2ea6d6e04686a5',
                       '6c845df28caa47e362ab0ec784fb64e9',
                       '66abb5e91095754d01bcb3f445494141', # lh.cortex.patch.3d
                       '47b92cb02f4b080b5602fef8d327e37d',
                       '27cdad54c091cb0ada615f5ac0b780b9'  # lh.thickness.nii.gz
  );

  md5sums_cifti = c('5aa64e146839f0332b5744d0a6e7f737',
                    'd9073d649d0a49e619fcf4e622a6d9df',
                    '66feaa6d0b5dabc448c8557426c8dbbb'
  );

  md5sums_nifti2 = '87524a733b65186a458fe2fc4a18041a';

  md5sums = c(md5sums_subject1, md5sums_cifti, md5sums_nifti2);

  ext_url_subject_part_subject1 = 'subjects_dir/subject1/';
  ext_url_parts_each_subject = c('label/lh.aparc.a2009s.annot',
                                 'label/lh.aparc.annot',
                                 'label/lh.cortex.label',
                                 'label/rh.aparc.a2009s.annot',
                                 'label/rh.aparc.annot',
                                 'label/rh.cortex.label',
                                 'mri/brain.mgz',
                                 'surf/lh.thickness',
                                 'surf/lh.white',
                                 'surf/rh.thickness',
                                 'surf/rh.white',
                                 'surf/lh.thickness.fwhm10.fsaverage.mgh',
                                 'surf/rh.thickness.fwhm10.fsaverage.mgh',
                                 'surf/lh.area',
                                 'surf/rh.area',
                                 'surf/lh.area.fwhm10.fsaverage.mgh',
                                 'surf/rh.area.fwhm10.fsaverage.mgh',
                                 'label/lh.aparc.a2005s.annot',
                                 'label/rh.aparc.a2005s.annot',
                                 'ext/lh.cortex.patch.3d',
                                 'ext/lh.cortex.patch.3d.asc',
                                 'surf/lh.thickness.nii.gz'
  );
  ext_urls_subject1 = paste(ext_url_subject_part_subject1, ext_url_parts_each_subject, sep='');

  ext_urls_cifti = c('cifti/Conte69.L.inflated.32k_fs_LR.surf.gii',
                     'cifti/Conte69.R.inflated.32k_fs_LR.surf.gii',
                     'cifti/Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii'
                     );

  ext_urls_internal_data = c(ext_urls_subject1, ext_urls_cifti);
  base_url_internal_data = 'http://rcmd.org/projects/nitestdata/'; # here 'internal' means data stored on our own rcmd.org server.
  internal_data_urls = paste(base_url_internal_data, ext_urls_internal_data, sep='');

  ext_urls_niftiv2 = 'avg152T1_LR_nifti2.nii.gz';
  base_url_nifti2_data = 'https://nifti.nimh.nih.gov/pub/dist/data/nifti2/';
  nifti2_data_urls = paste(base_url_nifti2_data, ext_urls_niftiv2, sep='');

  urls = c(internal_data_urls, nifti2_data_urls);

  cfiles = pkgfilecache::ensure_files_available(pkg_info, local_filenames, urls, md5sums=md5sums);
  cfiles$file_status = NULL; # not exposed to end user
  return(invisible(cfiles));
}




#' @title Get file names available in package cache.
#'
#' @description Get file names of optional data files which are available in the local package cache. You can access these files with get_optional_data_file().
#'
#' @return vector of strings. The file names available, relative to the package cache.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info list_available
list_opt_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("freesurferformats");
  return(pkgfilecache::list_available(pkg_info));
}


#' @title Access a single file from the package cache by its file name.
#'
#' @param filename, string. The filename of the file in the package cache.
#'
#' @param mustWork, logical. Whether an error should be created if the file does not exist. If mustWork=FALSE and the file does not exist, the empty string is returned.
#'
#' @return string. The full path to the file in the package cache or the empty string if there is no such file available. Use this in your application code to open the file.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info get_filepath
get_opt_data_filepath <- function(filename, mustWork=TRUE) {
  pkg_info = pkgfilecache::get_pkg_info("freesurferformats");
  return(pkgfilecache::get_filepath(pkg_info, filename, mustWork=mustWork));
}


#' @title Delete all data in the package cache.
#'
#' @return integer. The return value of the unlink() call: 0 for success, 1 for failure. See the unlink() documentation for details.
#'
#' @export
#' @importFrom pkgfilecache get_pkg_info erase_file_cache
delete_all_opt_data <- function() {
  pkg_info = pkgfilecache::get_pkg_info("freesurferformats");
  return(pkgfilecache::erase_file_cache(pkg_info));
}




