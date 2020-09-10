#' @title Determine whether a test is running on CRAN under macos
#'
#' @description We are currently getting failed unit tests on CRAN under macos, while the package works under MacOS on both <https://builder.r-hub.io/> and on our MacOS machines. This is because the package file cache does not work on CRAN, as the HOME is mounted read-only on the CRAN test systems. So we have to skip the tests that require optional data under MacOS on CRAN.
#'
#' @return logical, whether a test is running on CRAN under MacOS
tests_running_on_cran_under_macos <- function() {
  return(tolower(Sys.info()[["sysname"]]) == 'darwin' && !identical(Sys.getenv("NOT_CRAN"), "true"));
}


#' @title Check whether currently running R version is less than the given one.
rversion.less.than <- function(vmajor, vminor) {
  if(as.numeric(R.version$major) < vmajor) {
    return(TRUE);
  }
  if(as.numeric(R.version$major) == vmajor) {
    if(as.numeric(R.version$minor) < vminor) {
      return(TRUE);
    }
  }
  return(FALSE);
}


