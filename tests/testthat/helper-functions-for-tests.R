#' @title Determine whether a test is running on CRAN under macos
#'
#' @description We are currently getting failed unit tests on CRAN under macos, while the package works under MacOS on both <https://builder.r-hub.io/> and on our MacOS machines. This is because the package file cache does not work on CRAN, as the HOME is mounted read-only on the CRAN test systems. So we have to skip the tests that require optional data under MacOS on CRAN.
#'
#' @return logical, whether a test is running on CRAN under MacOS
tests_running_on_cran_under_macos <- function() {
  return(tolower(Sys.info()[["sysname"]]) == "darwin" && !identical(Sys.getenv("NOT_CRAN"), "true"))
}


#' @title Check whether currently running R version is less than the given one.
rversion.less.than <- function(vmajor, vminor) {
  if (as.numeric(R.version$major) < vmajor) {
    return(TRUE)
  }
  if (as.numeric(R.version$major) == vmajor) {
    if (as.numeric(R.version$minor) < vminor) {
      return(TRUE)
    }
  }
  return(FALSE)
}


#' @title Locate a file in the repository's extra_test_data directory.
#'
#' @description Some test data is too large to be shipped inside the package (R packages must stay
#' below 5 MB) and is therefore stored in the `extra_test_data` directory of the git repository,
#' which is excluded from the built and installed package via `.Rbuildignore`. This helper locates
#' a file in that directory when the tests run from a git checkout of the repository (local
#' development, or continuous integration that checks out the repo). It returns `NULL` when the
#' data is not available, i.e., on CRAN or for users who only have an installed copy of the
#' package. Use it together with `testthat::skip_if()` to skip tests that need the extra data.
#'
#' @param relpath character string, the path to the file relative to the `extra_test_data` directory.
#'
#' @return character string, the absolute path to the file, or `NULL` if the file was not found.
#'
#' @keywords internal
find_extra_test_data_file <- function(relpath) {
  candidate_dirs <- character(0)

  # Running the tests from the repository root (e.g., via devtools::test() or testthat::test_dir()).
  candidate_dirs <- c(candidate_dirs, file.path(getwd(), "extra_test_data"))

  # Running the tests from the package tests directory (tests/testthat), which is one level below the repo root.
  candidate_dirs <- c(candidate_dirs, file.path(getwd(), "..", "..", "extra_test_data"))

  # The working directory may differ from the repo root (e.g., when tests run from an installed
  # package). In that case the data location can be given explicitly via an environment variable.
  env_dir <- Sys.getenv("FREESURFERFORMATS_EXTRA_TEST_DATA", unset = "")
  if (nzchar(env_dir)) {
    candidate_dirs <- c(candidate_dirs, env_dir)
  }

  for (dir in candidate_dirs) {
    candidate <- file.path(dir, relpath)
    if (file.exists(candidate)) {
      return(normalizePath(candidate, mustWork = FALSE))
    }
  }
  return(NULL)
}
