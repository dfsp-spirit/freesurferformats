# Functions for converting from NIFTI to fs.volume instances.


# A note on the NIFTI coordinate system, from: 'Orientation informtion' on https://brainder.org/2012/09/23/the-nifti-file-format/
# "The world coordinate system is assumed to be ras: +x is Right, +y is Anterior and +z is Superior"


#' @title Resolve the path of a NIFTI file which is given without a file extension.
#'
#' @param filepath character string, the path to a NIFTI file.
#'
#' @return character string, the path of an existing file. If `filepath` itself does not exist, the usual
#'   NIFTI file extensions are appended to it and the first existing file is returned.
#'
#' @keywords internal
nifti.resolve.filepath <- function(filepath) {
  if (file.exists(filepath)) {
    return(filepath)
  }

  # NIFTI files are often referred to by their base name, without the file extension, so the usual NIFTI file
  # extensions are tried as well. This is the order in which they were tried before this function existed
  # (by oro.nifti::readNIfTI).
  if (filepath.ends.with(filepath, c(".nii", ".nii.gz", ".hdr", ".hdr.gz", ".img", ".img.gz"))) {
    stop(sprintf("Cannot read NIFTI file '%s': the file does not exist.\n", filepath))
  }

  candidates <- paste0(filepath, c(".nii.gz", ".nii", ".hdr.gz", ".img.gz", ".hdr", ".img"))
  for (candidate in candidates) {
    if (file.exists(candidate)) {
      return(candidate)
    }
  }

  stop(sprintf("Cannot read NIFTI file '%s': the file does not exist, and neither does any of the files '%s'.\n", filepath, paste(candidates, collapse = "', '")))
}


#' @title Read the header information and the data of a NIFTI file into a plain list.
#'
#' @description This is the file reading part of \code{\link{read.fs.volume.nii}}. The returned list uses plain
#'   entry names instead of the `oro.nifti` slot names, so that no code outside of this file depends on the
#'   name of an `oro.nifti` slot. The data scaling fields of the NIFTI header are applied, see the details in
#'   \code{\link{read.fs.volume.nii}}.
#'
#' @param filepath character string, the path to a NIFTI v1 or v2 file. The file extension may be omitted, see
#'   \code{\link{nifti.resolve.filepath}}.
#'
#' @return named list with the NIFTI header fields `magic`, `datatype`, `bitpix`, `dim` (the 8 entry NIFTI
#'   `dim` field, its first entry the number of used dimensions), `glmin` (the field used to store the true
#'   dimension for the FreeSurfer hack files, `-1` for NIFTI v2 files, which do not have it), `scl_slope`,
#'   `scl_inter`, `xyzt_units`, `pixdim` (the 8 entry NIFTI `pixdim` field, its first entry `qfac`),
#'   `sform_code`, `srow_x`, `srow_y`, `srow_z`, `qform_code`, `quatern_b`, `quatern_c`, `quatern_d`,
#'   `qoffset_x`, `qoffset_y`, `qoffset_z`, and the data array in `data`.
#'
#' @seealso \code{\link{nifti.info.from.oro.instance}}
#'
#' @keywords internal
nifti.info.from.file <- function(filepath) {
  filepath <- nifti.resolve.filepath(filepath)

  version <- nifti.file.version(filepath)
  if (is.null(version)) {
    stop(sprintf("File '%s' is not a NIFTI file: its first 4 bytes are neither a NIFTI v1 header size (348) nor a NIFTI v2 header size (540).\n", filepath))
  }

  if (version == 1L) {
    header <- read.nifti1.header(filepath)
    if (!(header$magic == "n+1" | header$magic == "ni1")) {
      # A 348 byte header without the NIFTI magic string does not belong to a NIFTI file, it is an ANALYZE 7.5
      # header. Reading such a file as NIFTI would silently return the wrong data, so it is refused here.
      stop(sprintf("Unknown NIFTI magic code '%s', file format not supported. Expected magic code 'n+1' or 'ni1'.\n", header$magic))
    }
    data <- read.nifti1.data(filepath, header = header, drop_empty_dims = FALSE)
    glmin <- header$glmin
  } else {
    header <- read.nifti2.header(filepath)
    data <- read.nifti2.data(filepath, header = header, drop_empty_dims = FALSE)
    glmin <- -1L # NIFTI v2 has no 'glmin' field, and thus no FreeSurfer hack.
  }

  nii <- list(
    magic = header$magic, datatype = header$datatype, bitpix = header$bitpix, dim = header$dim,
    glmin = glmin, scl_slope = header$scl_slope, scl_inter = header$scl_inter,
    xyzt_units = header$xyzt_units, pixdim = header$pix_dim, sform_code = header$sform_code,
    srow_x = header$srow_x, srow_y = header$srow_y, srow_z = header$srow_z,
    qform_code = header$qform_code, quatern_b = header$quatern_b, quatern_c = header$quatern_c,
    quatern_d = header$quatern_d, qoffset_x = header$qoffset_x, qoffset_y = header$qoffset_y,
    qoffset_z = header$qoffset_z, data = data
  )

  # 'oro.nifti' replaces non-finite voxel sizes, and voxel sizes of 0 for the used data dimensions, with 1 while
  # reading a file. This is replicated here to keep the returned header unchanged (see the note in the docs of
  # read.fs.volume.nii), even though reporting the value stored in the file would be more accurate.
  bad_pixdim <- !is.finite(nii$pixdim)
  if (any(bad_pixdim)) {
    nii$pixdim[bad_pixdim] <- 1.
  }
  if (nii$dim[1] >= 1L && nii$dim[1] <= 7L) { # For other values the caller stops with an error anyway, see read.fs.volume.nii.
    used_dims <- 2:(1 + nii$dim[1]) # the used data dimensions are 'dim[2]' to 'dim[1 + dim[1]]'
    bad_pixdim <- nii$pixdim[used_dims] == 0.
    if (any(bad_pixdim)) {
      nii$pixdim[used_dims][bad_pixdim] <- 1.
    }
  }

  # NIFTI files can store the data in a different value range than the one the data type implies, and describe the
  # transformation in the 'scl_slope' and 'scl_inter' header fields. This function read the files with oro.nifti
  # before, which applies that transformation while reading by default ('rescale_data = TRUE') and then resets the
  # fields in the returned instance. The same is done here, so that the data returned for a file are unchanged.
  # The NIFTI standard says that no transformation is to be applied if 'scl_slope' is 0, and oro.nifti behaves that
  # way as well.
  if (nii$scl_slope != 0.0 && !(nii$scl_slope == 1.0 && nii$scl_inter == 0.0)) {
    nii$data <- nii$data * nii$scl_slope + nii$scl_inter
    nii$scl_slope <- 1.0
    nii$scl_inter <- 0.0
  }

  return(nii)
}


#' @title Extract the header information and the data of an `oro.nifti` instance into a plain list.
#'
#' @param nifti_img an instance of class `nifti` from the `oro.nifti` package.
#'
#' @return named list, see \code{\link{nifti.info.from.file}}.
#'
#' @note The data scaling fields of the instance are not applied here, they are expected to be applied already:
#'   instances read by `oro.nifti::readNIfTI` are rescaled unless `rescale_data = FALSE` was used.
#'
#' @seealso \code{\link{nifti.info.from.file}}
#'
#' @keywords internal
nifti.info.from.oro.instance <- function(nifti_img) {
  return(list(
    magic = nifti_img@magic, datatype = nifti_img@datatype, bitpix = nifti_img@bitpix, dim = nifti_img@dim_,
    glmin = nifti_img@glmin, scl_slope = nifti_img@scl_slope, scl_inter = nifti_img@scl_inter,
    xyzt_units = nifti_img@xyzt_units, pixdim = nifti_img@pixdim, sform_code = nifti_img@sform_code,
    srow_x = nifti_img@srow_x, srow_y = nifti_img@srow_y, srow_z = nifti_img@srow_z,
    qform_code = nifti_img@qform_code, quatern_b = nifti_img@quatern_b, quatern_c = nifti_img@quatern_c,
    quatern_d = nifti_img@quatern_d, qoffset_x = nifti_img@qoffset_x, qoffset_y = nifti_img@qoffset_y,
    qoffset_z = nifti_img@qoffset_z, data = nifti_img@.Data
  ))
}


#' @title Read a 3D or 4D NIFTI file into an `fs.volume` instance with complete header.
#'
#' @description This function reads a NIFTI v1 or v2 file, or takes a `nifti` instance from the `oro.nifti`
#'   package, and computes the MGH header fields from the NIFTI header data, allowing for proper orientation of
#'   the contained image data (see \code{\link[freesurferformats]{mghheader.vox2ras}} and related functions).
#'   Files are read with the NIFTI reader of this package, so the `oro.nifti` package is only needed if a
#'   `nifti` instance is passed, or if `reorient` or extra arguments are used. Currently only few datatypes are
#'   supported, and the orientation can only be derived if the `sform` or `qform` header field is present.
#'
#' @param filepath instance of class `nifti` from the `oro.nifti` package, or a path to a NIFTI file as a character string.
#'
#' @inheritParams read.fs.mgh
#'
#' @param do_rotate logical, whether to rotate 3D volumes to compensate for storage order. WIP.
#'
#' @param reorient logical, whether to let \code{oro.nifti::readNIfTI} reorient the data array to a standard orientation while reading it from a file. Defaults to `FALSE`, see the note below. Only relevant if `filepath` is a path, it is ignored if a `nifti` instance is passed. Using `TRUE` requires the `oro.nifti` package.
#'
#' @param ... extra parameters passed to \code{oro.nifti::readNIfTI}. Leave this alone unless you know what you are doing. Note that `reorient` is passed explicitly by this function, so it cannot be set here. Passing any extra parameter makes the file be read by `oro.nifti` instead of by the NIFTI reader of this package, and thus requires the `oro.nifti` package.
#'
#' @return an `fs.volume` instance. The `header` fields are computed from the NIFTI header. The `data` array is returned in the raw NIFTI file storage order (first dimension fastest), which is also the order used by the MGH/MGZ format. For a file, the NIFTI data scaling fields `scl_slope`/`scl_inter` of the header are applied to the values while reading (this is what the `oro.nifti` package does for files as well), for an `oro.nifti` instance the values are returned as they are stored in the instance. If the NIFTI file contains `sform` or `qform` geometry information, the returned header contains a `vox2ras_matrix` entry in addition to the MGH header fields, just like the header returned by \code{\link[freesurferformats]{read.fs.mgh}}.
#'
#' @seealso \code{oro.nifti::readNIfTI}, \code{\link[freesurferformats]{read.fs.mgh}}
#'
#' @note The data array is *not* reoriented, because the NIFTI geometry is stored in the `sform`/`qform` header fields, and these describe the raw file storage order. Reorientation as performed by `oro.nifti::readNIfTI(reorient = TRUE)` permutes or flips the data array without updating the `sform` fields, so the data array would no longer match the header of the returned `fs.volume` (and thus not the `vox2ras_matrix` used to map voxel indices to coordinates). Using `reorient = TRUE`, or passing a `nifti` instance that was read with `reorient = TRUE`, is therefore discouraged and results in a warning.
#'
#' @note Files are read with the NIFTI reader of this package, which reads both NIFTI v1 and NIFTI v2 files and does not require the `oro.nifti` package. Two details of the way `oro.nifti` used to read files are kept, so that the returned values do not change: the NIFTI data scaling fields (`scl_slope`/`scl_inter`) are applied to the values while reading (see the return value section), and voxel sizes that are stored as 0 for a used dimension (or that are not finite) are reported as 1.
#'
#' @note This is not supposed to be used to read 1D morphometry data from NIFTI files generated by FreeSurfer (e.g., by converting `lh.thickness` to NIFTI using `mri_convert`): such files contain a single dimension, and the volume returned for them is degenerate. Use \code{\link[freesurferformats]{read.fs.morph}} to read morphometry data.
#'
#' @references See https://nifti.nimh.nih.gov/nifti-1/ for the NIfTI-1 data format spec.
#'
#' @examples
#' \dontrun{
#' base_file <- "~/data/subject1_only/subject1/mri/brain"
#' # missing file ext.
#' mgh_file <- paste(base_file, ".mgz", sep = "")
#' # the standard MGH/MGZ file
#' nii_file <- paste(base_file, ".nii", sep = "")
#' # NIFTI file generated with mri_convert
#' brain_mgh <- read.fs.mgh(mgh_file, with_header = TRUE)
#' brain_nii <- read.fs.volume.nii(nii_file, with_header = TRUE)
#' all(brain_nii$data == brain_mgh$data)
#' # output: TRUE
#' all(mghheader.vox2ras(brain_nii) == mghheader.vox2ras(brain_mgh)) # output: TRUE
#' }
#'
#' @export
read.fs.volume.nii <- function(filepath, flatten = FALSE, with_header = FALSE, drop_empty_dims = FALSE, do_rotate = FALSE, reorient = FALSE, ...) {
  if (is.character(filepath)) {
    # Files are read with the NIFTI reader of this package. The 'oro.nifti' package is only used to read the file
    # if the caller asked for something our reader does not do: reorienting the data while reading it, or passing
    # extra arguments through to 'oro.nifti::readNIfTI'.
    if (reorient || length(list(...)) > 0L) {
      if (!requireNamespace("oro.nifti", quietly = TRUE)) {
        stop("The 'oro.nifti' package is required to read a NIFTI file with 'reorient = TRUE' or with extra arguments passed to 'oro.nifti::readNIfTI'. Install it, or read the file with the default arguments.\n")
      }
      if (reorient) {
        warning("Reorienting the data array while reading it permutes or flips the voxel data without updating the 'sform'/'qform' header fields, so the returned data will not match the returned geometry (see 'mghheader.vox2ras'). Reading without reorientation is strongly recommended.\n")
      }
      nii <- nifti.info.from.oro.instance(oro.nifti::readNIfTI(filepath, reorient = reorient, ...))
      if (!(nii$magic == "n+1" | nii$magic == "ni1")) {
        stop(sprintf("Unknown NIFTI magic code '%s', file format not supported. Expected magic code 'n+1' or 'ni1'.\n", nii$magic))
      }
    } else {
      nii <- nifti.info.from.file(filepath)
    }
  } else {
    if (!requireNamespace("oro.nifti", quietly = TRUE)) {
      stop("The 'oro.nifti' package is required to use a 'nifti' instance: pass a filepath instead, or install the package.\n")
    }
    if (!oro.nifti::is.nifti(filepath)) {
      stop("Parameter 'filepath' must be a path to a NIFTI file or a nifti instance from the 'oro.nifti' package.")
    }
    # A nifti instance that was reoriented on read has a data array that does not match its 'sform' header
    # fields, so the geometry derived below would not describe the data.
    if (isTRUE(filepath@reoriented)) {
      warning("The nifti instance was reoriented when it was read (its 'reoriented' slot is TRUE), so its voxel data may not match its 'sform'/'qform' header fields anymore. Read the file with 'reorient = FALSE', or pass the filepath instead of the instance.\n")
    }
    nii <- nifti.info.from.oro.instance(filepath)
    if (!(nii$magic == "n+1" | nii$magic == "ni1")) {
      stop(sprintf("Unknown NIFTI magic code '%s', file format not supported. Expected magic code 'n+1' or 'ni1'.\n", nii$magic))
    }
  }

  ## --------------------- Perform some basic sanity checks on the Nifti header. ---------------------
  scale_data <- FALSE # whether data scaling is required. This is scaling based on the scaling part in the header, not to be confused with scaling due to time/space units.
  if (nii$scl_slope != 0.0) {
    if (!(nii$scl_slope == 1.0 & nii$scl_inter == 0.0)) {
      scale_data <- TRUE
    }
  }
  if (scale_data) {
    # This can only happen for a 'nifti' instance which was read by oro.nifti with 'rescale_data = FALSE':
    # for files the scaling is applied while reading (see nifti.info.from.file), and oro.nifti resets the
    # fields in the instances it returns.
    warning(sprintf("Detected that Nifti data needs scaling (@scl_slope=%.2f, @scl_inter=%.2f), but scaling not implemented yet.\n", nii$scl_slope, nii$scl_inter))
  }

  # The intent code describes how to interprete the data (e.g., that the values describe a certain distribution or whatever).
  # See https://brainder.org/2012/09/23/the-nifti-file-format/ or the NIFTI spec for details.
  # There is no field for this in MGH header afaict, so we ignore it.
  # if(nii$intent_code != 0L) {
  #  message("NIFTI intent_code '%d' ignored.\n", nii$intent_code);
  # }

  ## -----Check the datatype ------
  MRI_UCHAR <- translate.mri.dtype("MRI_UCHAR")
  MRI_INT <- translate.mri.dtype("MRI_INT")
  MRI_FLOAT <- translate.mri.dtype("MRI_FLOAT")
  MRI_SHORT <- translate.mri.dtype("MRI_SHORT")

  # the datatype is the NIFTI data type integer code
  dti <- nifti.dtype.info(nii$datatype, nii$bitpix)
  dtype <- dti$mri_dtype

  # bytes_per_voxel = mri_dtype_numbytes(dtype); # Note that we store the size in **bytes** per voxel, while the Nifti header uses **bits**.
  # message(sprintf("Nifti header: Nifti datatype=%d with %d bitpix. MRI datatype '%s' (code %d), with %d bytes per voxel.\n", nii$datatype, nii$bitpix, translate.mri.dtype(dtype), dtype, bytes_per_voxel));

  ## ----- Check image dimensions -----
  num_used_dimensions <- nii$dim[1]
  byte_swap <- FALSE
  if (num_used_dimensions < 1L | num_used_dimensions > 7L) {
    # If the first field is outside the range 1-7, it means that the data has opposite endianness and must be byte-swapped.
    # The NIFTI readers of this package detect the endianness of a file, so this should never happen.
    byte_swap <- TRUE # should never happen
    stop(sprintf("Byte-swapped Nifti images not supported yet.\n"))
  }

  # If the image has only 3 dimensions, set frame count to 1.
  num_frames <- ifelse(num_used_dimensions < 4L, 1L, nii$dim[5]) # The first entry of the 'dim' field is the number of "used" dimensions, the data dimensions follow it.


  # If the image has more than 4 dimensions, we do not support it yet.
  if (num_used_dimensions > 4L & nii$dim[6] > 1L) {
    stop("Nifti images with more than 4 used dimensions not supported yet.")
  }

  # Check for FreeSurfer hack in number of columns (negative column count, true column count stored in the 'glmin' field).
  # The header readers of this package resolve the hack while reading the header, so this may never happen.
  if (nii$dim[2] < 0L) {
    # message(sprintf("Nifti image with FreeSurfer hack detected, assuming %d columns.\n", nii$glmin));
    ncols <- nii$glmin
  } else {
    ncols <- nii$dim[2]
  }

  # ico7_num_vertices = 163842L; # Vertex count of ICO 7 meshes, like fsaverage. If the dimensions match this, the file is assumed to
  #                              contain morphometry data stored with the FreeSurfer hack.
  # is_ico7 = (ncols * nii$dim[3] * nii$dim[4] == ico7_num_vertices);
  # ico7_state_string = ifelse(is_ico7, "looks like", "does NOT look like");
  # message(sprintf("Nifti header: Image has %d used dimensions. It %s ico7 morphometry data.\n", nii$dim[1], ico7_state_string));

  ## Compute space and time unit factors from xyzt_units.
  # NIFTI can store in different units, while MGH uses fixed units. Depending on the unit used in the NIFTI, we may need to
  # rescale the values to match the MGH unit. (This is not to be confused with the data scaling fields in the NIFTI header, see scl_slope and scl_inter handling above).
  space_info <- nifti.space.info(nii$xyzt_units)
  space_unit_factor <- space_info$scaling
  time_info <- nifti.time.info(nii$xyzt_units)
  time_unit_factor <- time_info$scaling
  # message(sprintf("Space data in NIFTI file is stored in unit '%s', using scaling factor %f to match FreeSurfer unit 'mm'.\n", space_info$name, space_info$scaling));
  # message(sprintf("Time data in NIFTI file is stored in unit '%s', using scaling factor %f to match FreeSurfer unit 'ms'.\n", time_info$name, time_info$scaling));


  header <- mghheader(c(ncols, nii$dim[3], nii$dim[4], num_frames), dtype)
  header$internal$xsize <- nii$pixdim[2] # voxel size in x direction. Still needs scaling by space_unit_factor, see below.
  header$internal$ysize <- nii$pixdim[3] # voxel size in y direction. Still needs scaling by space_unit_factor, see below.
  header$internal$zsize <- nii$pixdim[4] # voxel size in z direction. Still needs scaling by space_unit_factor, see below.
  header$internal$tr <- nii$pixdim[5] # TR. Still needs scaling by time_unit_factor, see below.

  # Compute the vox2ras transformation. This is the crucial part.
  # See the 'Orientation information' section on https://brainder.org/2012/09/23/the-nifti-file-format/ for interpretation
  # and a good overview.
  if (nii$sform_code != 0L) { # Means that the sform is present in the srow_x, srow_y and srow_z header fields.
    # Extract vox2ras matrix from NIFTI sform, then init the MGH header from the vox2ras matrix.
    vox2ras <- rbind(nii$srow_x, nii$srow_y, nii$srow_z, c(0, 0, 0, 1))
    header <- mghheader.update.from.vox2ras(header, vox2ras)
    # message(sprintf("Computed transformation matrix into '%s' using sform header data.\n", nifti.transform.type.name(nii$sform_code)));
    header$ras_good_flag <- 1L
  } else if (nii$qform_code != 0L) {
    # The qform transform is based on the 3 qoffset fields, qfac, and 4 quaternions. 3 of them are in the following header fields, the 4th one needs to be
    # computed from the other 3.
    qb <- nii$quatern_b
    qc <- nii$quatern_c
    qd <- nii$quatern_d

    # Compute qa
    qa <- 1.0 - (qb * qb + qc * qc + qd * qd)
    if (qa < 1.0e-7) {
      qa <- 1.0 / sqrt(qb * qb + qc * qc + qd * qd)
      qb <- qa * qb
      qc <- qa * qc
      qd <- qa * qd
      qa <- 0.0
    } else {
      qa <- sqrt(qa)
    }

    # Now construct the 3x3 rotation matrix.
    rot_mat <- matrix(rep(0., 9), nrow = 3L)
    rot_mat[1, 1] <- qa * qa + qb * qb - qc * qc - qd * qd
    rot_mat[1, 2] <- 2.0 * qb * qc - 2.0 * qa * qd
    rot_mat[1, 3] <- 2.0 * qb * qd + 2.0 * qa * qc
    rot_mat[2, 1] <- 2.0 * qb * qc + 2.0 * qa * qd
    rot_mat[2, 2] <- qa * qa + qc * qc - qb * qb - qd * qd
    rot_mat[2, 3] <- 2.0 * qc * qd - 2.0 * qa * qb
    rot_mat[3, 1] <- 2.0 * qb * qd - 2.0 * qa * qc
    rot_mat[3, 2] <- 2.0 * qc * qd + 2.0 * qa * qb
    rot_mat[3, 3] <- qa * qa + qd * qd - qc * qc - qb * qb

    # Now use voxel sizes and translation vector to compute final transform
    qfac <- nii$pixdim[1]
    if (!(qfac == -1 | qfac == 1)) { # We're in the dangerous world of floating point comparison here.
      warning(sprintf("Treating non-standard qfac value '%f' as 1.0\n.", qfac)) # R is good at it, but I would rather let the user know if anything looks suspicious.
      qfac <- 1
    }
    rot_mat[1, 3] <- rot_mat[1, 3] * qfac
    rot_mat[2, 3] <- rot_mat[2, 3] * qfac
    rot_mat[3, 3] <- rot_mat[3, 3] * qfac

    # Fill in header fields.
    header$internal$x_r <- rot_mat[1, 1]
    header$internal$y_r <- rot_mat[1, 2]
    header$internal$z_r <- rot_mat[1, 3]
    header$internal$x_a <- rot_mat[2, 1]
    header$internal$y_a <- rot_mat[2, 2]
    header$internal$z_a <- rot_mat[2, 3]
    header$internal$x_s <- rot_mat[3, 1]
    header$internal$y_s <- rot_mat[3, 2]
    header$internal$z_s <- rot_mat[3, 3]

    # Compute and set center RAS
    header$internal$c_r <- (header$internal$xsize * header$internal$x_r) * (header$internal$width / 2.0) +
      (header$internal$ysize * header$internal$y_r) * (header$internal$height / 2.0) +
      (header$internal$zsize * header$internal$z_r) * (header$internal$depth / 2.0) + nii$qoffset_x

    header$internal$c_a <- (header$internal$xsize * header$internal$x_a) * (header$internal$width / 2.0) +
      (header$internal$ysize * header$internal$y_a) * (header$internal$height / 2.0) +
      (header$internal$zsize * header$internal$z_a) * (header$internal$depth / 2.0) + nii$qoffset_y

    header$internal$c_s <- (header$internal$xsize * header$internal$x_s) * (header$internal$width / 2.0) +
      (header$internal$ysize * header$internal$y_s) * (header$internal$height / 2.0) +
      (header$internal$zsize * header$internal$z_s) * (header$internal$depth / 2.0) + nii$qoffset_z
    header$ras_good_flag <- 1L


    # message(sprintf("Computed transformation matrix into '%s' using qform header data.\n", nifti.transform.type.name(nii$qform_code)));
  } else {
    warning("Nifti image does not contain valid sform or qform, orientation cannot be derived and is arbitrary.")
    # Fill in more or less random orientation values, scale voxel values by xsize, ysize, zsize.
    header$internal$x_r <- -1.0
    header$internal$x_a <- 0.0
    header$internal$x_s <- 0.0
    header$internal$y_r <- 0.0
    header$internal$y_a <- 1.0
    header$internal$y_s <- 0.0
    header$internal$z_r <- 0.0
    header$internal$z_a <- 0.0
    header$internal$z_s <- 1.0
    header$internal$c_r <- header$internal$xsize * header$internal$width / 2.0
    header$internal$c_a <- header$internal$ysize * header$internal$height / 2.0
    header$internal$c_s <- header$internal$zsize * header$internal$depth / 2.0
    # Indicate missing RAS info:
    header$ras_good_flag <- 0L
  }

  header$internal$xsize <- header$internal$xsize * space_unit_factor
  header$internal$ysize <- header$internal$ysize * space_unit_factor
  header$internal$zsize <- header$internal$zsize * space_unit_factor
  header$internal$c_r <- header$internal$c_r * space_unit_factor
  header$internal$c_a <- header$internal$c_a * space_unit_factor
  header$internal$c_s <- header$internal$c_s * space_unit_factor
  header$internal$tr <- header$internal$tr * time_unit_factor

  header$has_mr_params <- 1L
  header$mr <- list()
  header$mr$tr <- header$internal$tr

  # TODO: Refactor: The next block is duplicated from read.fs.mgh and should go into a separate function.
  x_half_length <- header$internal$width / 2.0 * header$internal$xsize
  y_half_length <- header$internal$height / 2.0 * header$internal$ysize
  z_half_length <- header$internal$depth / 2.0 * header$internal$zsize
  header$internal$xstart <- -x_half_length
  header$internal$xend <- x_half_length
  header$internal$ystart <- -y_half_length
  header$internal$yend <- y_half_length
  header$internal$zstart <- -z_half_length
  header$internal$zend <- z_half_length
  xfov <- header$internal$xend - header$internal$xstart
  yfov <- header$internal$yend - header$internal$ystart
  zfov <- header$internal$zend - header$internal$zstart
  header$internal$fov <- ifelse(xfov > yfov, ifelse(xfov > zfov, xfov, zfov), ifelse(yfov > zfov, yfov, zfov))
  header$mr$fov <- header$internal$fov
  orientation_info <- get.slice.orientation(header$internal$Mdc)
  header$internal$slice_orientation_string <- orientation_info$orientation_string
  header$internal$slice_direction_name <- orientation_info$direction_name

  # Add the vox2ras matrix as a convenience field, like read.fs.mgh does. This is only possible if the
  # NIFTI file contained geometry information (an sform or qform header field).
  if (mghheader.is.ras.valid(header)) {
    header$vox2ras_matrix <- mghheader.vox2ras(header)
  }

  data <- nii$data

  # Check in which storage ordering the data is saved in the NIFTI image and rotate/permute the array accordingly.
  # See https://brainder.org/2012/09/23/the-nifti-file-format/ and the official NIFTI standard.

  if (do_rotate) {
    if (length(dim(drop(data))) == 3L) {
      data <- rotate3D(drop(data), axis = 1L, degrees = 90L)
    } else {
      warning(sprintf("Not rotating: data dimension is '%s'.\n", paste(dim(data), collapse = " ")))
    }
  }

  if (length(dim(data)) != 4) {
    # Most likely the 4th dimension of size 1 is missing, reshape it.
    dim(data) <- c(ncols, nii$dim[3], nii$dim[4], num_frames)
  }

  nv <- prod(dim(data)) # number of voxels
  if (flatten) {
    dim(data) <- c(nv)
    data <- as.vector(unlist(data))
    header$voldim <- c(length(data))
  }

  if (drop_empty_dims) {
    data <- drop(data)
  }

  fsvol <- list("header" = header, "data" = data)
  class(fsvol) <- "fs.volume"
  if (with_header) {
    return(fsvol)
  } else {
    return(data)
  }
}


#' @title Compute NIFTI space unit info from xyzt_units header field.
#'
#' @param xyzt_units a single character, the `xyzt_units` NIFTI header field
#'
#' @return named list with entries: `code`: the NIFTI unit code as a decimal integer, `name`: character string, the unit name, `scaling`: float, the scaling factor for the unit, relative to the FreeSurfer space unit (`mm`).
#'
#' @keywords internal
nifti.space.info <- function(xyzt_units) {
  nifti_unit_code <- bitwAnd(xyzt_units, 0x07)
  nifti_unit_name <- "unknown"
  scaling <- 1.0
  if (nifti_unit_code == 1L) {
    nifti_unit_name <- "m"
    scaling <- 1000.0
  }
  if (nifti_unit_code == 2L) {
    nifti_unit_name <- "mm"
    scaling <- 1.0
  }
  if (nifti_unit_code == 3L) {
    nifti_unit_name <- "mum"
    scaling <- 0.001
  }
  return(list("code" = nifti_unit_code, "name" = nifti_unit_name, "scaling" = scaling))
}


#' @title Compute NIFTI v1 data type info from datatype and bitpix header field.
#'
#' @param datatype integer, the `datatype` NIFTI v1 header field
#'
#' @param bitpix integer, the `bitpix` NIFTI v1 header field
#'
#' @return named list with entries: `mri_dtype`: the MRI data type, as used by FreeSurfer for MGH files, `r_dtype`: the R data type, `size`: the number of bytes per value, `signed`: logical, whether the values are signed (only meaningful for integer types, `NA` for floating point types) and `is_float`: logical, whether the type is a floating point type.
#'
#' @note The `signed` entry matters for reading the data: an unsigned 8 bit value of 200 is read as -56 if it is read as a signed value, which is a silent change of the data. See \code{\link{read.nifti.values}}.
#'
#' @keywords internal
nifti.dtype.info <- function(datatype, bitpix) {
  MRI_UCHAR <- translate.mri.dtype("MRI_UCHAR")
  MRI_INT <- translate.mri.dtype("MRI_INT")
  MRI_FLOAT <- translate.mri.dtype("MRI_FLOAT")
  MRI_SHORT <- translate.mri.dtype("MRI_SHORT")
  if (datatype == 2L & bitpix == 8L) { # NIFTI: 'unsigned char'
    return(list("mri_dtype" = MRI_UCHAR, "r_dtype" = integer(), "size" = 1L, "signed" = FALSE, "is_float" = FALSE))
  } else if (datatype == 4L & bitpix == 16L) { # NIFTI: 'signed short'
    return(list("mri_dtype" = MRI_SHORT, "r_dtype" = integer(), "size" = 2L, "signed" = TRUE, "is_float" = FALSE))
  } else if (datatype == 8L & bitpix == 32L) { # NIFTI: 'signed int'
    return(list("mri_dtype" = MRI_INT, "r_dtype" = integer(), "size" = 4L, "signed" = TRUE, "is_float" = FALSE))
  } else if (datatype == 512L & bitpix == 16L) { # NIFTI: 'unsigned short', we map this to MRI_INT
    return(list("mri_dtype" = MRI_INT, "r_dtype" = integer(), "size" = 2L, "signed" = FALSE, "is_float" = FALSE))
  } else if (datatype == 768L & bitpix == 32L) { # NIFTI: 'unsigned int', we map this to MRI_INT and print a notice
    return(list("mri_dtype" = MRI_INT, "r_dtype" = integer(), "size" = 4L, "signed" = FALSE, "is_float" = FALSE))
  } else if (datatype == 16L & bitpix == 32L) { # NIFTI: 'float'
    return(list("mri_dtype" = MRI_FLOAT, "r_dtype" = numeric(), "size" = 4L, "signed" = NA, "is_float" = TRUE))
  } else if (datatype == 64L & bitpix == 64L) { # NIFTI: 'double', but we treat this as MRI_FLOAT, there is no double support for MGH afaik.
    return(list("mri_dtype" = MRI_FLOAT, "r_dtype" = numeric(), "size" = 8L, "signed" = NA, "is_float" = TRUE))
  } else {
    stop(sprintf("Nifti images with datatype=%d and bitpix=%d not supported yet.\n", datatype, bitpix))
  }
}


#' @title Get the name of the transform type from a form code.
#'
#' @description The form code is a code stored in the `sform_code` and/or `qform_code` NIFTI header fields.
#'
#' @param form_code integer, the value retrieved from the `sform_code` or the `qform_code` NIFTI header fields
#'
#' @return character string, the meaning of the code. Usually this expresses to what the data will be aligned after application of the vox2ras transformation method. (The type of transformation to perform in order to achieve this alignment depends on whether the value was retrieved from the `sform` or the `qform` field and does not matter here.)
#'
#' @keywords internal
nifti.transform.type.name <- function(form_code) {
  if (form_code == 1L) {
    return("scanner_anatomical")
  }
  if (form_code == 2L) {
    return("reference")
  }
  if (form_code == 3L) {
    return("talairach_space")
  }
  if (form_code == 4L) {
    return("MNI152_space")
  }
  return("unknown")
}


#' @title Compute NIFTI time unit info from xyzt_units header field.
#'
#' @param xyzt_units a single character, the `xyzt_units` NIFTI header field
#'
#' @return named list with entries: `code`: the NIFTI unit code as a decimal integer, `name`: character string, the unit name, `scaling`: float, the scaling factor for the unit, relative to the FreeSurfer time unit  (`ms`).
#'
#' @keywords internal
nifti.time.info <- function(xyzt_units) {
  nifti_unit_code <- bitwAnd(xyzt_units, 0x38)
  nifti_unit_name <- "unknown"
  scaling <- 1.0
  if (nifti_unit_code == 8L) {
    nifti_unit_name <- "s"
    scaling <- 1000.0
  }
  if (nifti_unit_code == 16L) {
    nifti_unit_name <- "ms"
    scaling <- 1.0
  }
  if (nifti_unit_code == 24L) {
    nifti_unit_name <- "mus"
    scaling <- 0.001
  }
  return(list("code" = nifti_unit_code, "name" = nifti_unit_name, "scaling" = scaling))
}


#' @title Compute the 'datatype' and 'bitpix' fields used in the NIFTI1 header from an MGH/MGZ datatype code.
#'
#' @param mgh_dtype_code integer, the MGH/MGZ datatype code (as returned by `translate.mri.dtype`).
#'
#' @note This is useful to compute a NIFTI v1 header from an MGH header.
#'
#' @return named list with entries: `datatype` and `bitpix` containing the translated data for the respective NIfTI-1 header fields.
#'
#' @keywords internal
nifti.dtypebitpix.info.from.mgh.dtype <- function(mgh_dtype_code) {
  MRI_UCHAR <- translate.mri.dtype("MRI_UCHAR")
  MRI_INT <- translate.mri.dtype("MRI_INT")
  MRI_FLOAT <- translate.mri.dtype("MRI_FLOAT")
  MRI_SHORT <- translate.mri.dtype("MRI_SHORT")

  if (mgh_dtype_code == MRI_UCHAR) {
    datatype <- 2L
    bitpix <- 8L
  } else if (mgh_dtype_code == MRI_INT) {
    datatype <- 8L
    bitpix <- 32L
  } else if (mgh_dtype_code == MRI_FLOAT) {
    datatype <- 16L
    bitpix <- 32L
  } else if (mgh_dtype_code == MRI_SHORT) {
    datatype <- 4L
    bitpix <- 16L
  } else {
    stop(sprintf("Invalid mgh_dtype_code '%d' encountered when trying to convert MGH header datatype info to NIFTI header datatype info.\n", mgh_dtype_code))
  }

  res <- list("datatype" = datatype, "bitpix" = bitpix)
  return(res)
}
