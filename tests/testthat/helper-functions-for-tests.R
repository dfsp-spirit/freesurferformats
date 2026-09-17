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


#' @title Write a minimal TRK file for testing.
#'
#' @description Writes a version 2 TRK file with the given tracks, so that tests
#' do not depend on downloaded optional data. Per-point scalars and per-track
#' properties are filled with predictable values when requested:
#' scalar number `s` (1-based) of point `j` of track `t` is `1000 * s + j`, and
#' property `p` (1-based) of track `t` is `10000 * t + p`.
#'
#' @param path character string, the output file path.
#' @param tracks list of numeric matrices with 3 columns, the tracks.
#' @param endian character string, 'little' or 'big'.
#' @param vox2ras 4x4 numeric matrix, the matrix stored in the header.
#' @param n_count integer or NULL, the value for the 'n_count' header field. NULL
#'   stores the real number of tracks, 0L stores "unknown".
#' @param n_scalars integer, number of scalars per point.
#' @param n_properties integer, number of properties per track.
#' @param voxel_size numeric vector of length 3.
#' @param voxel_order character string, stored in the header.
#'
#' @return the path, invisibly.
write_test_trk <- function(path, tracks, endian = "little", vox2ras = diag(4), n_count = NULL,
                           n_scalars = 0L, n_properties = 0L, voxel_size = c(1, 1, 1),
                           voxel_order = "LPS") {
  con <- file(path, "wb")
  on.exit({ close(con) }, add = TRUE)

  w_i16 <- function(values) writeBin(as.integer(values), con, size = 2, endian = endian)
  w_i32 <- function(values) writeBin(as.integer(values), con, size = 4, endian = endian)
  w_f32 <- function(values) writeBin(as.numeric(values), con, size = 4, endian = endian)
  w_chr <- function(value, n) {
    bytes <- charToRaw(value)
    writeBin(c(bytes, as.raw(rep(0, n - length(bytes))))[seq_len(n)], con)
  }

  w_chr("TRACK", 6)
  w_i16(c(10, 10, 10))          # dim
  w_f32(voxel_size)             # voxel_size
  w_f32(c(0, 0, 0))             # origin
  w_i16(n_scalars)
  w_chr("", 200)                # scalar_names
  w_i16(n_properties)
  w_chr("", 200)                # property_names
  w_f32(as.numeric(t(vox2ras))) # vox_to_ras
  w_chr("", 444)                # reserved
  w_chr(voxel_order, 4)
  w_chr("", 4)                  # pad2
  w_f32(rep(0, 6))              # image_orientation_patient
  w_chr("", 2)                  # pad1
  writeBin(as.raw(rep(0, 6)), con) # invert_x/y/z, swap_xy/yz/zx
  w_i32(if (is.null(n_count)) length(tracks) else n_count)
  w_i32(2L)                     # version
  w_i32(1000L)                  # hdr_size

  for (track_idx in seq_along(tracks)) {
    track <- tracks[[track_idx]]
    num_points <- nrow(track)
    w_i32(num_points)
    for (point_idx in seq_len(num_points)) {
      w_f32(track[point_idx, ])
      if (n_scalars > 0L) {
        w_f32(1000 * seq_len(n_scalars) + point_idx)
      }
    }
    if (n_properties > 0L) {
      w_f32(10000 * track_idx + seq_len(n_properties))
    }
  }

  return(invisible(path))
}


#' @title Write a minimal TCK file for testing.
#'
#' @description Writes a single-file TCK file with the given tracks, so that
#' tests do not depend on downloaded optional data. The payload is written the
#' way the MRtrix and nibabel writers do it, with a NaN triplet after every
#' track and an Inf triplet at the end; see the arguments to test the other
#' variants.
#'
#' @param path character string, the output file path.
#' @param tracks list of numeric matrices with 3 columns, the tracks.
#' @param datatype character string, one of 'Float32LE', 'Float32BE',
#'   'Float64LE', 'Float64BE'.
#' @param extra_header_lines character vector of additional header lines.
#' @param terminator logical, whether to write the Inf triplet at the end.
#' @param count_key logical, whether to write a 'count' header entry.
#' @param count_value integer or NULL, the value to write for the 'count' header
#'   entry. NULL writes the real number of tracks.
#' @param nan_after_last logical, whether to write a NaN triplet after the last
#'   track (TRUE, as MRtrix and nibabel do) or only between tracks (FALSE).
#'
#' @return the path, invisibly.
write_test_tck <- function(path, tracks, datatype = "Float32LE", extra_header_lines = character(0),
                           terminator = TRUE, count_key = TRUE, count_value = NULL,
                           nan_after_last = TRUE) {
  dsize <- if (grepl("64", datatype)) 8L else 4L
  endian <- if (endsWith(datatype, "BE")) "big" else "little"

  values <- numeric(0)
  for (track_idx in seq_along(tracks)) {
    values <- c(values, as.numeric(t(tracks[[track_idx]])))
    if (track_idx < length(tracks) || nan_after_last) {
      values <- c(values, NaN, NaN, NaN)
    }
  }
  if (terminator) {
    values <- c(values, Inf, Inf, Inf)
  }

  # The offset depends on its own number of digits, so iterate until it is stable.
  offset <- 100L
  repeat {
    lines <- c("mrtrix tracks",
               if (count_key) sprintf("count: %d", if (is.null(count_value)) length(tracks) else count_value),
               paste0("datatype: ", datatype),
               extra_header_lines,
               sprintf("file: . %d", offset),
               "END")
    header <- paste0(paste(lines, collapse = "\n"), "\n")
    new_offset <- nchar(header, type = "bytes")
    if (new_offset == offset) {
      break
    }
    offset <- new_offset
  }

  con <- file(path, "wb")
  writeBin(charToRaw(header), con)
  writeBin(as.numeric(values), con, size = dsize, endian = endian)
  close(con)
  return(invisible(path))
}


#' @title Write a minimal TSF file for testing.
#'
#' @description Writes a single-file TSF file with the given per-track values,
#' using one NaN per track boundary and one Inf at the end.
#'
#' @param path character string, the output file path.
#' @param track_values list of numeric vectors, the values of each track.
#' @param datatype character string, one of 'Float32LE', 'Float32BE',
#'   'Float64LE', 'Float64BE'.
#'
#' @return the path, invisibly.
write_test_tsf <- function(path, track_values, datatype = "Float32LE") {
  dsize <- if (grepl("64", datatype)) 8L else 4L
  endian <- if (endsWith(datatype, "BE")) "big" else "little"

  values <- numeric(0)
  for (track_idx in seq_along(track_values)) {
    values <- c(values, as.numeric(track_values[[track_idx]]), NaN)
  }
  values <- c(values, Inf)

  offset <- 100L
  repeat {
    lines <- c("mrtrix track scalars",
               sprintf("count: %d", length(track_values)),
               paste0("datatype: ", datatype),
               "timestamp: 12345.0",
               sprintf("file: . %d", offset),
               "END")
    header <- paste0(paste(lines, collapse = "\n"), "\n")
    new_offset <- nchar(header, type = "bytes")
    if (new_offset == offset) {
      break
    }
    offset <- new_offset
  }

  con <- file(path, "wb")
  writeBin(charToRaw(header), con)
  writeBin(as.numeric(values), con, size = dsize, endian = endian)
  close(con)
  return(invisible(path))
}


#' @title Deterministic pseudo-random tracks for testing.
#'
#' @param num_tracks integer, number of tracks to generate.
#' @param points_per_track integer, number of points per track.
#' @param seed integer, random seed.
#'
#' @return list of numeric matrices with 3 columns.
make_test_tracks <- function(num_tracks, points_per_track, seed = 1L) {
  set.seed(seed)
  return(replicate(num_tracks,
                   cbind(stats::runif(points_per_track, 0, 100),
                         stats::runif(points_per_track, 0, 100),
                         stats::runif(points_per_track, 0, 100)),
                   simplify = FALSE))
}


#' @title Write a gradient table file for testing.
#'
#' @description Writes the given lines to a file in a fresh temporary directory.
#' The contents are given as a character vector, one string per line, so that
#' the tests can exercise all on-disk layouts (three component lines versus one
#' volume per line, a single row versus one value per line).
#'
#' @param lines character vector, the lines of the file.
#' @param name character string, the file name to use.
#'
#' @return character string, the path to the written file.
write_test_gradient_file <- function(lines, name = "grad.b") {
  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  path <- file.path(tmp_dir, name)
  writeLines(lines, path)
  return(path)
}


#' @title Write a pair of gradient table files for testing.
#'
#' @description Writes a b-vectors file and a b-values file into the same fresh
#' temporary directory, which is what the automatic lookup of the b-values file
#' needs.
#'
#' @param bvec_lines character vector, the lines of the b-vectors file.
#' @param bval_lines character vector, the lines of the b-values file.
#' @param bvec_name character string, the file name of the b-vectors file.
#' @param bval_name character string, the file name of the b-values file.
#'
#' @return named list with the entries 'dir' (the temporary directory), 'bvec'
#'   and 'bval' (the two file paths).
write_test_gradient_pair <- function(bvec_lines, bval_lines, bvec_name = "dwi.bvec", bval_name = "dwi.bval") {
  tmp_dir <- tempfile("dwigrad")
  dir.create(tmp_dir)
  bvec_path <- file.path(tmp_dir, bvec_name)
  bval_path <- file.path(tmp_dir, bval_name)
  writeLines(bvec_lines, bvec_path)
  writeLines(bval_lines, bval_path)
  return(list("dir" = tmp_dir, "bvec" = bvec_path, "bval" = bval_path))
}

