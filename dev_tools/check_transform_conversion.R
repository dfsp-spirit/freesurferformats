# Cross-validate the FSL transformation conversion of this package against two independent implementations.
#
# The package converts an FSL/FLIRT matrix ('*.mat') into a world transformation, see
# 'transform.to.world()'. FSL does not use the world space of the image header for this, but a space with unit
# voxel axes and a flipped first axis, so the conversion is not obvious and must be verified against reference
# implementations rather than derived from first principles.
#
# This script checks our result against:
#   1. FreeSurfer's lta_convert (--infsl ... --outlta ...), which writes the world transformation as a RAS2RAS
#      LTA file. Requires the FSLOUTPUTTYPE environment variable to be set (NIFTI or ANALYZE), since FreeSurfer
#      uses it to decide the flipping.
#   2. MRtrix3's transformconvert (... flirt_import ...), which writes the inverse of the world transformation.
#
# Both tools are optional: the script reports which of them it could not find and skips that check. Run it with
#   Rscript dev_tools/check_transform_conversion.R
# and use the environment variables FREESURFER_HOME / MRTRIX_BIN to point to non-standard installations.

freesurfer_bin <- Sys.getenv("FREESURFER_HOME", unset = path.expand("~/software/freesurfer/freesurfer7.4.1"))
lta_convert_bin <- file.path(freesurfer_bin, "bin", "lta_convert")
mrtrix_bin <- Sys.getenv("MRTRIX_BIN", unset = path.expand("~/software/micromamba/envs/mrtrix3/bin"))
transformconvert_bin <- file.path(mrtrix_bin, "transformconvert")

script_args <- commandArgs(trailingOnly = FALSE)
script_file <- sub("^--file=", "", script_args[grep("^--file=", script_args)])
repo_root <- if (length(script_file) == 1L) normalizePath(file.path(dirname(script_file), ".."), mustWork = FALSE) else getwd()
if (!file.exists(file.path(repo_root, "DESCRIPTION"))) {
  repo_root <- getwd()
}
devtools::load_all(repo_root, quiet = TRUE)

read_matrix_file <- function(filepath) {
  lines <- readLines(filepath)
  lines <- lines[!startsWith(lines, "#")]
  matrix(as.numeric(unlist(strsplit(trimws(lines), "[[:space:]]+"))), ncol = 4L, byrow = TRUE)
}

# Compare against a reference implementation. The tolerance is relative to the magnitude of the reference
# matrix, because the tools do not all compute in double precision: FreeSurfer's lta_convert works in single
# precision (about 1e-7 relative), while MRtrix3 uses double precision (about 1e-12 relative).
compare_with_reference <- function(transform_name, tool_label, our_matrix, reference_matrix, tolerance) {
  usable <- (!is.null(reference_matrix) && is.matrix(reference_matrix) &&
    all(dim(reference_matrix) == c(4L, 4L)) && all(is.finite(reference_matrix)))
  if (!usable) {
    cat(sprintf("   [%-16s] %-28s : SKIPPED, the reference tool produced no usable matrix\n", transform_name, tool_label))
    return("skipped")
  }
  absolute_difference <- max(abs(reference_matrix - our_matrix))
  relative_difference <- absolute_difference / max(1.0, max(abs(reference_matrix)))
  status <- if (relative_difference < tolerance) "OK" else "MISMATCH"
  cat(sprintf(
    "   [%-16s] %-28s : max abs diff %.3g (relative %.3g) %s\n",
    transform_name, tool_label, absolute_difference, relative_difference, status
  ))
  return(status)
}

tally <- function(status, checks, failures) {
  if (status == "OK") {
    checks <- checks + 1L
  } else if (status == "MISMATCH") {
    checks <- checks + 1L
    failures <- failures + 1L
  }
  return(list("checks" = checks, "failures" = failures))
}

# Image pairs to test with. Different voxel sizes exercise the scaling, a positive determinant exercises the
# flip, and an oblique direction matrix exercises the axis handling.
image_pairs <- list(
  list(
    name = "example data of the package (1mm brain.mgz, oblique NIfTI)",
    src = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE),
    dst = system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)
  ),
  list(
    name = "real subject (2mm DWI, 3mm MNI BOLD)",
    src = path.expand("~/develop/sub-01/bids/sub-01/dwi/sub-01_dwi.nii.gz"),
    dst = path.expand("~/develop/sub-01-derived/bids/derivatives/fmriprep/sub-01/func/sub-01_task-rest_space-MNI152NLin2009cAsym_desc-preproc_bold.nii.gz")
  )
)

transforms <- list(
  "identity" = diag(4),
  "translation" = { m <- diag(4); m[1:3, 4] <- c(3, -2, 4); m },
  "scaling" = diag(c(2, 1, 1, 1)),
  "rotation" = { a <- 10 * pi / 180; m <- diag(4); m[1, 1] <- cos(a); m[1, 2] <- -sin(a); m[2, 1] <- sin(a); m[2, 2] <- cos(a); m },
  "offset_flag" = { m <- diag(4); m[1:3, 4] <- c(-30, 12, 7); m[2, 2] <- 0.98; m },
  "determinant_zero" = diag(c(1, 1, 0, 1))
)

has_lta_convert <- file.exists(lta_convert_bin)
has_transformconvert <- file.exists(transformconvert_bin)
cat(sprintf("FreeSurfer lta_convert: %s\n", if (has_lta_convert) lta_convert_bin else "NOT FOUND, skipped"))
cat(sprintf("MRtrix3 transformconvert: %s\n", if (has_transformconvert) transformconvert_bin else "NOT FOUND, skipped"))

work_dir <- tempfile("transform_conversion_check_")
dir.create(work_dir, recursive = TRUE)
failures <- 0L
checks <- 0L

for (pair in image_pairs) {
  if (!file.exists(pair$src) || !file.exists(pair$dst)) {
    cat(sprintf("\n## Skipping image pair '%s': an image is missing.\n", pair$name))
    next
  }
  src_volume <- read.fs.volume(pair$src, with_header = TRUE)
  dst_volume <- read.fs.volume(pair$dst, with_header = TRUE)
  src_geom <- volume.geometry(src_volume)
  dst_geom <- volume.geometry(dst_volume)

  cat(sprintf("\n## Image pair: %s\n", pair$name))
  cat(sprintf("   src: %s voxels, %s mm, det %s\n", paste(src_geom$dim, collapse = "x"),
              paste(src_geom$voxelsize, collapse = "x"), signif(det(src_geom$vox2ras[1:3, 1:3]), 6)))
  cat(sprintf("   dst: %s voxels, %s mm, det %s\n", paste(dst_geom$dim, collapse = "x"),
              paste(dst_geom$voxelsize, collapse = "x"), signif(det(dst_geom$vox2ras[1:3, 1:3]), 6)))

  for (transform_name in names(transforms)) {
    matrix <- transforms[[transform_name]]
    mat_file <- file.path(work_dir, sprintf("%s_%s.mat", gsub("[^a-zA-Z0-9]", "_", pair$name), transform_name))
    writeLines(apply(matrix, 1L, function(r) paste(sprintf("%.12g", r), collapse = " ")), mat_file)

    our_transform <- transform.to.world(read.fs.transform(mat_file), src = src_volume, dst = dst_volume)

    # 1. FreeSurfer: the LTA file holds the world transformation directly.
    if (has_lta_convert) {
      lta_file <- file.path(work_dir, "freesurfer_out.lta")
      if (file.exists(lta_file)) {
        unlink(lta_file)
      }
      lta_convert_output <- system2(
        lta_convert_bin,
        c("--infsl", mat_file, "--outlta", lta_file, "--src", pair$src, "--trg", pair$dst),
        stdout = TRUE, stderr = TRUE, env = "FSLOUTPUTTYPE=NIFTI"
      )
      if (!file.exists(lta_file) || file.info(lta_file)$size == 0L) {
        cat(sprintf("   [%-16s] %-28s : SKIPPED, lta_convert failed (%s)\n", transform_name, "vs FreeSurfer lta_convert", paste(tail(lta_convert_output, 1L), collapse = " ")))
      } else {
        reference <- tryCatch(read.fs.transform(lta_file)$matrix, error = function(e) NULL)
        # FreeSurfer computes in single precision and the translations of these matrices reach a few hundred
        # millimeters, so a relative tolerance of 1e-5 is used here. It still catches convention errors, which
        # are of order 1 (a missing flip or a missing voxel size division changes the result by tens of mm).
        status <- compare_with_reference(transform_name, "vs FreeSurfer lta_convert", our_transform$matrix, reference, 1e-5)
        counters <- tally(status, checks, failures)
        checks <- counters$checks
        failures <- counters$failures
      }
    }

    # 2. MRtrix3: the transform file holds the inverse of the world transformation.
    if (has_transformconvert) {
      mrtrix_out <- file.path(work_dir, "mrtrix_out.txt")
      if (file.exists(mrtrix_out)) {
        unlink(mrtrix_out)
      }
      mrtrix_console <- system2(
        transformconvert_bin,
        c(mat_file, pair$src, pair$dst, "flirt_import", mrtrix_out),
        stdout = TRUE, stderr = TRUE
      )
      if (!file.exists(mrtrix_out) || file.info(mrtrix_out)$size == 0L) {
        cat(sprintf("   [%-16s] %-28s : SKIPPED, transformconvert failed (%s)\n", transform_name, "vs MRtrix3 flirt_import", paste(tail(mrtrix_console, 1L), collapse = " ")))
      } else {
        # MRtrix3 writes the inverse of the world transformation, see its source code.
        reference <- tryCatch(solve(read_matrix_file(mrtrix_out)), error = function(e) NULL)
        status <- compare_with_reference(transform_name, "vs MRtrix3 flirt_import", our_transform$matrix, reference, 1e-9)
        counters <- tally(status, checks, failures)
        checks <- counters$checks
        failures <- counters$failures
      }
    }

    # 3. Internal consistency: converting back must return the matrix we started with.
    round_trip <- max(abs(transform.to.voxel(our_transform)$matrix - matrix))
    checks <- checks + 1L
    status <- if (round_trip < 1e-10) "OK" else { failures <- failures + 1L; "MISMATCH" }
    cat(sprintf("   [%-16s] %-28s : max abs difference %.3g  %s\n", transform_name, "round trip to voxel and back", round_trip, status))
  }
}

# --------------------------------------------------------------------------------------------------------------
# Writer checks: write each format with this package, then let FreeSurfer read both the file we wrote and the
# original file it was derived from. Converting both to a RAS2RAS LTA must give the same transformation, which
# shows that the file we wrote states the same thing as the file FreeSurfer wrote itself.
# --------------------------------------------------------------------------------------------------------------
example_files <- list(
  "lta" = system.file("extdata", "talairach.lta", package = "freesurferformats", mustWork = TRUE),
  "dat" = system.file("extdata", "register.dat", package = "freesurferformats", mustWork = TRUE),
  "xfm" = system.file("extdata", "talairach.xfm", package = "freesurferformats", mustWork = TRUE)
)
write_geometry_args <- function(format, image_a, image_b) {
  if (format == "lta") {
    return(character(0)) # an LTA file records the geometry of its volumes itself
  }
  return(c("--src", image_a, "--trg", image_b))
}
input_flag <- c("lta" = "--inlta", "dat" = "--inreg", "xfm" = "--inmni")

writer_example_src <- system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE)
writer_example_dst <- system.file("extdata", "vol27int.nii.gz", package = "freesurferformats", mustWork = TRUE)

cat("\n## Writer checks\n")
for (format in names(example_files)) {
  original_file <- example_files[[format]]
  tf <- read.fs.transform(original_file)
  written_file <- file.path(work_dir, sprintf("written.%s", format))
  write.fs.transform(tf, written_file, format = format)

  # This package must read back exactly what it wrote.
  reread_matrix <- read.fs.transform(written_file)$matrix
  difference <- max(abs(reread_matrix - tf$matrix))
  checks <- checks + 1L
  status <- if (difference < 1e-15) "OK" else { failures <- failures + 1L; "MISMATCH" }
  cat(sprintf("   [%-16s] %-28s : max abs diff %.3g  %s\n", format, "own read back", difference, status))

  if (!has_lta_convert) {
    next
  }

  # FreeSurfer must interpret our file like the original one. Both are converted to a RAS2RAS LTA, which is the
  # canonical form of a transformation in FreeSurfer, and the two results are compared.
  converted_files <- character(0)
  conversion_failed <- FALSE
  for (source_kind in c("original", "written")) {
    input_file <- if (source_kind == "original") original_file else written_file
    converted_file <- file.path(work_dir, sprintf("converted_from_%s_%s.lta", source_kind, format))
    if (file.exists(converted_file)) {
      unlink(converted_file)
    }
    console_output <- system2(
      lta_convert_bin,
      c(input_flag[format], input_file, "--outlta", converted_file, write_geometry_args(format, writer_example_src, writer_example_dst)),
      stdout = TRUE, stderr = TRUE, env = "FSLOUTPUTTYPE=NIFTI"
    )
    if (!file.exists(converted_file) || file.info(converted_file)$size == 0L) {
      cat(sprintf("   [%-16s] %-28s : SKIPPED, lta_convert could not read the %s file (%s)\n",
                  format, "vs FreeSurfer lta_convert", source_kind, paste(tail(console_output, 1L), collapse = " ")))
      conversion_failed <- TRUE
    } else {
      converted_files <- c(converted_files, converted_file)
    }
  }
  if (!conversion_failed && length(converted_files) == 2L) {
    reference <- tryCatch(read.fs.transform(converted_files[1L])$matrix, error = function(e) NULL)
    ours <- tryCatch(read.fs.transform(converted_files[2L])$matrix, error = function(e) NULL)
    status <- compare_with_reference(format, "vs FreeSurfer lta_convert", ours, reference, 1e-5)
    counters <- tally(status, checks, failures)
    checks <- counters$checks
    failures <- counters$failures
  }
}

# The FSL matrix writer must agree with FreeSurfer's own FSL writer for the same LTA.
if (has_lta_convert) {
  lta_file <- example_files[["lta"]]
  tf <- read.fs.transform(lta_file)
  ours_file <- file.path(work_dir, "ours.mat")
  theirs_file <- file.path(work_dir, "theirs.mat")
  write.fs.transform(tf, ours_file, format = "fslmat")
  system2(
    lta_convert_bin,
    c("--inlta", lta_file, "--outfsl", theirs_file),
    stdout = TRUE, stderr = TRUE, env = "FSLOUTPUTTYPE=NIFTI"
  )
  if (!file.exists(theirs_file) || file.info(theirs_file)$size == 0L) {
    cat(sprintf("   [%-16s] %-28s : SKIPPED, lta_convert --outfsl failed\n", "fslmat", "vs FreeSurfer --outfsl"))
  } else {
    reference <- tryCatch(read.fs.transform(theirs_file)$matrix, error = function(e) NULL)
    status <- compare_with_reference("fslmat", "vs FreeSurfer --outfsl", read.fs.transform(ours_file)$matrix, reference, 1e-6)
    counters <- tally(status, checks, failures)
    checks <- counters$checks
    failures <- counters$failures
  }
}

unlink(work_dir, recursive = TRUE)
cat(sprintf("\n%d checks performed, %d failures.\n", checks, failures))
if (failures > 0L) {
  stop("Transformation conversion checks failed.\n")
}
cat("All transformation conversion checks passed.\n")