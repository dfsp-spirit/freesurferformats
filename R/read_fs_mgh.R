#' @title Read file in FreeSurfer MGH or MGZ format
#'
#' @description Read multi-dimensional brain imaging data from a file in FreeSurfer binary MGH or MGZ format. The MGZ format is just a gzipped version of the MGH format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/mri/T1.mgz', which contains a 3D brain scan of bert.
#'
#' @param filepath, string. Full path to the input MGZ or MGH file.
#'
#' @param is_gzipped, a logical value (TRUE or FALSE) or the string 'AUTO'. Whether to treat the input file as gzipped, i.e., MGZ instead of MGH format. Defaults to 'AUTO', which tries to determine this from the last three characters of the 'filepath' parameter. Files with extensions 'mgz' and '.gz' (in arbitrary case) are treated as MGZ format, all other files are treated as MGH. In the special case that 'filepath' has less than three characters, MGH is assumed.
#'
#' @param flatten, logical. Whether to flatten the return volume to a 1D vector. Useful if you know that this file contains 1D morphometry data.
#'
#' @return data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header. If the parameter flatten is TRUE, a numeric vector is returned instead.
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vd = read.fs.mgh(brain_image);
#'     cat(sprintf("Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
#'                  paste(dim(vd), collapse = ' '), min(vd), mean(vd), max(vd)));
#'
#' @export
read.fs.mgh <- function(filepath, is_gzipped = "AUTO", flatten = FALSE) {

    if(typeof(is_gzipped) == "logical") {
        is_gz = is_gzipped;
    } else if (typeof(is_gzipped) == "character") {
        if(is_gzipped == "AUTO") {
            is_gz = guess.filename.is.gzipped(filepath);
        } else {
            stop("Argument 'is_gzipped' must be 'AUTO' if it is a string.\n");
        }
    } else {
        stop(sprintf("ERROR: Argument is_gzipped must be logical (TRUE or FALSE) or 'AUTO'.\n"));
    }

    if (is_gz) {
        fh = gzfile(filepath, "rb");
    }
    else {
        fh = file(filepath, "rb");
    }

    v = readBin(fh, integer(), n = 1, endian = "big");
    ndim1 = readBin(fh, integer(), n = 1, endian = "big");
    ndim2 = readBin(fh, integer(), n = 1, endian = "big");
    ndim3  = readBin(fh, integer(), n = 1, endian = "big");
    nframes = readBin(fh, integer(), n = 1, endian = "big");
    dtype = readBin(fh, integer(), n = 1, endian = "big");
    dof = readBin(fh, integer(), n = 1, endian = "big");

    unused_header_space_size_left = 256;

    ras_good_flag = readBin(fh, integer(), size = 2, n = 1, endian = "big");
    if(ras_good_flag == 1) {
        delta  = readBin(fh, numeric(), n = 3, size = 4, endian = "big");
        Mdc    = readBin(fh, numeric(), n = 9, size = 4, endian = "big");
        Pxyz_c = readBin(fh, numeric(), n = 3, size = 4, endian = "big");
        RAS_space_size = (3*4 + 4*3*4);    # 60 bytes
        unused_header_space_size_left = unused_header_space_size_left - RAS_space_size;
    }

    # Skip to end of header/beginning of data
    seek(fh, where = unused_header_space_size_left, origin = "current");

    nv = ndim1 * ndim2 * ndim3 * nframes;   # number of voxels
    volsz = c(ndim1, ndim2, ndim3, nframes);

    # Determine size of voxel data, depending on dtype from header above
    MRI_UCHAR = 0;
    MRI_INT = 1;
    MRI_FLOAT = 3;
    MRI_SHORT = 4;

    dt_explanation = "0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 3=MRI_SHORT";

    # Determine number of bytes per voxel
    if(dtype == MRI_FLOAT) {
        nbytespervox = 4;
        data = readBin(fh, numeric(), size = nbytespervox, n = nv, endian = "big");
    } else if(dtype == MRI_UCHAR) {
        nbytespervox = 1;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, signed = FALSE, endian = "big");
    } else if (dtype == MRI_SHORT) {
        nbytespervox = 2;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, endian = "big");
    } else if (dtype == MRI_INT) {
        nbytespervox = 4;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, endian = "big");
    } else {
       stop(sprintf(" ERROR: Unexpected data type found in header. Expected one of {0, 1, 3, 4} (%s) but got %d.\n", dt_explanation, dtype));
    }

    num_read = prod(length(data));
    if (num_read != nv) {
        stop(sprintf(" ERROR: read %d voxel values but expected to read %d.\n", num_read, nv));
    }

    # Reshape to expected dimensions
    data = array(data, dim = c(ndim1, ndim2, ndim3, nframes));

    if(flatten) {
        dim(data) = c(nv);
        data = as.vector(unlist(data));
    }

    close(fh);
    return(data);
}


#' @title Guess whether a file is gzipped.
#'
#' @description Guess whether a file is gzipped, based on the file extension.
#'
#' @param filepath, string. Path to a file.
#'
#' @param gz_entensions, list of strings. A list of suffixes that is considered indicative for the file being gzipped. Defaults to c(".gz", ".mgz").
#'
#' @return logical, whether this function thinks the file is gzipped.
#'
#' @keywords internal
guess.filename.is.gzipped <- function(filepath, gz_entensions=c(".gz", ".mgz")) {
    nc = nchar(filepath);
    for (gz_ext in gz_entensions) {
        num_chars_to_inspect = nchar(gz_ext);
        if(nc >= num_chars_to_inspect) {
            ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1, nchar(filepath));
            if(tolower(ext) == gz_ext) {
                return(TRUE);
            }
        }
    }
    return(FALSE);
}
