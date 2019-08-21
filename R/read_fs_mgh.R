#' @title Read file in FreeSurfer MGH or MGZ format
#'
#' @description Read multi-dimensional brain imaging data from a file in FreeSurfer binary MGH or MGZ format. The MGZ format is just a gzipped version of the MGH format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/mri/T1.mgz', which contains a 3D brain scan of bert.
#'
#' @param filepath, string. Full path to the input MGZ or MGH file.
#'
#' @param is_gzipped, a logical value (TRUE or FALSE) or the string 'AUTO'. Whether to treat the input file as gzipped, i.e., MGZ instead of MGH format. Defaults to 'AUTO', which tries to determine this from the last three characters of the 'filepath' parameter. Files with extensions 'mgz' and '.gz' (in arbitrary case) are treated as MGZ format, all other files are treated as MGH. In the special case that 'filepath' has less than three characters, MGZ is assumed.
#'
#' @return data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header.
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
#'     voxel_data = read.fs.mgh(brain_image);
#'     cat(sprintf("Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",  paste(dim(voxel_data), collapse = ' '), min(voxel_data), mean(voxel_data), max(voxel_data)));
#'
#' @export
read.fs.mgh <- function(filepath, is_gzipped = "AUTO") {

    if(typeof(is_gzipped) == "logical") {
        is_gz = is_gzipped;
    } else if (typeof(is_gzipped) == "character") {
        if(is_gzipped == "AUTO") {
            nc = nchar(filepath);
            num_chars_to_inspect = 3; # last 3 chars
            if(nc >= 3) {
                ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1, nchar(filepath));
                if(tolower(ext) == "mgz" || tolower(ext) == ".gz") {
                    is_gz = TRUE;
                } else {
                    is_gz = FALSE;
                }
            } else {
                warning(sprintf("Argument 'is_gzipped set' to 'AUTO' but file name is too short (%d chars) to determine compression from last %d characters, assuming gz-compressed file.\n", nc, num_chars_to_inspect));
                is_gz = TRUE;
            }
        } else {
            stop("Argument 'is_gzipped' must be 'AUTO' if it is a string.\n");
        }
    } else {
        stop(sprintf("ERROR: Argument is_gzipped must be logical (TRUE or FALSE) or 'AUTO'.\n"));
    }

    if (is_gz) {
         cat(sprintf("*Parsing header of file '%s', assuming it is gz-compressed.\n", filepath));
        fh = gzfile(filepath, "rb");
    }
    else {
        cat(sprintf("*Parsing header of file '%s', assuming it is NOT gz-compressed.\n", filepath));
        fh = file(filepath, "rb");
    }

    v = readBin(fh, integer(), n = 1, endian = "big");
    ndim1 = readBin(fh, integer(), n = 1, endian = "big");
    ndim2 = readBin(fh, integer(), n = 1, endian = "big");
    ndim3  = readBin(fh, integer(), n = 1, endian = "big");
    nframes = readBin(fh, integer(), n = 1, endian = "big");
    dtype = readBin(fh, integer(), n = 1, endian = "big");
    dof = readBin(fh, integer(), n = 1, endian = "big");


    cat(sprintf(" v=%d, ndim1=%d, ndim2=%d, ndim3=%d, nframes=%d, type=%d, dof=%d.\n", v, ndim1, ndim2, ndim3, nframes, dtype, dof));


    unused_header_space_size_left = 256;

    ras_good_flag = readBin(fh, numeric(), n = 1, endian = "big");
    if(ras_good_flag == 1) {
        cat(sprintf(" 'RAS good' flag is set, reading RAS information.\n"));
        delta  = readBin(filehandle, float(), n = 3, endian = "big");
        Mdc    = readBin(filehandle, float(), n = 9, endian = "big");
        Pxyz_c = readBin(filehandle, float(), n = 3, endian = "big");
        RAS_space_size = (3*4 + 4*3*4);
        cat(sprintf(" Read %d bytes of RAS information.\n", RAS_space_size));
        unused_header_space_size_left = unused_header_space_size_left - RAS_space_size;
    } else {
        cat(sprintf(" 'RAS good' flag is NOT set, no RAS information to read.\n"));
    }
    cat(sprintf(" There are %d unused header bytes left that will be skipped (data starts at fixed index).\n", unused_header_space_size_left));

    # Skip to end of header/beginning of data
    seek(fh, where = unused_header_space_size_left, origin = "current");

    nv = ndim1 * ndim2 * ndim3 * nframes;   # number of voxels
    volsz = c(ndim1, ndim2, ndim3, nframes);
    cat(sprintf(" Expecting %d voxels total.\n", nv));

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
        data = readBin(fh, int(), size = nbytespervox, n = nv, endian = "big");
    } else {
       stop(sprintf(" ERROR: Unexpected data type found in header. Expected one of {0, 1, 3, 4} (%s) but got %d.\n", dt_explanation, dtype));
    }
    cat(sprintf(" Data type found in header is %d (%s), with %d bytes per voxel.\n", dtype, dt_explanation, nbytespervox));

    num_read = prod(length(data));
    if (num_read == nv) {
        cat(sprintf(" OK, read %d voxel values as expected.\n", num_read));
    } else {
        cat(sprintf(" ERROR: read %d voxel values but expected to read %d.\n", num_read, nv));
        quit(status=1);
    }

    # Reshape to expected dimensions
    data = array(data, dim = c(ndim1, ndim2, ndim3, nframes));
    cat(sprintf(" Reshaped array to expected dimensions: %s\n", paste(dim(data), collapse = ' ')));
    close(fh);
    return(data);
}
