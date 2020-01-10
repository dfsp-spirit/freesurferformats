#' @title Read file in FreeSurfer MGH or MGZ format
#'
#' @description Read multi-dimensional brain imaging data from a file in FreeSurfer binary MGH or MGZ format. The MGZ format is just a gzipped version of the MGH format. For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/mri/T1.mgz', which contains a 3D brain scan of bert.
#'
#' @param filepath string. Full path to the input MGZ or MGH file.
#'
#' @param is_gzipped a logical value (TRUE or FALSE) or the string 'AUTO'. Whether to treat the input file as gzipped, i.e., MGZ instead of MGH format. Defaults to 'AUTO', which tries to determine this from the last three characters of the 'filepath' parameter. Files with extensions 'mgz' and '.gz' (in arbitrary case) are treated as MGZ format, all other files are treated as MGH. In the special case that 'filepath' has less than three characters, MGH is assumed.
#'
#' @param flatten logical. Whether to flatten the return volume to a 1D vector. Useful if you know that this file contains 1D morphometry data.
#'
#' @param with_header logical. Whether to return the header as well. If TRUE, return a named list with entries "data" and "header". The latter is another named list which contains the header data. These header entries exist: "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions. E.g., c(256, 256, 256, 1). These header entries may exist: "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists if "has_mr_params" is 1).
#'
#' @param drop_empty_dims logical, whether to drop empty dimensions of the returned data
#'
#' @return data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header. If the parameter flatten is TRUE, a numeric vector is returned instead. Note: The return value changes if the parameter with_header is TRUE, see parameter description.
#'
#' @family morphometry functions
#'
#' @examples
#'     brain_image = system.file("extdata", "brain.mgz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     vd = read.fs.mgh(brain_image);
#'     cat(sprintf("Read voxel data with dimensions %s. Values: min=%d, mean=%f, max=%d.\n",
#'                  paste(dim(vd), collapse = ' '), min(vd), mean(vd), max(vd)));
#'     # Read it again with full header data:
#'     vdh = read.fs.mgh(brain_image, with_header = TRUE);
#'     # Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
#'     vdh$header$vox2ras_matrix %*% c(0,0,0,1);
#'
#' @export
read.fs.mgh <- function(filepath, is_gzipped = "AUTO", flatten = FALSE, with_header=FALSE, drop_empty_dims=FALSE) {


    header = list();

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
    if(v!=1L) {
        stop("File not in MGH/MGZ format.");
    }
    ndim1 = readBin(fh, integer(), n = 1, endian = "big");
    ndim2 = readBin(fh, integer(), n = 1, endian = "big");
    ndim3  = readBin(fh, integer(), n = 1, endian = "big");
    nframes = readBin(fh, integer(), n = 1, endian = "big");
    dtype = readBin(fh, integer(), n = 1, endian = "big");
    dof = readBin(fh, integer(), n = 1, endian = "big");

    header$dtype = dtype;
    header$dof = dof;
    header$internal = list();

    unused_header_space_size_left = 256;

    header$ras_good_flag = readBin(fh, integer(), size = 2, n = 1, endian = "big");
    if(header$ras_good_flag == 1) {
        delta = readBin(fh, numeric(), n = 3, size = 4, endian = "big");
        Mdc = readBin(fh, numeric(), n = 9, size = 4, endian = "big");
        Mdc = matrix(Mdc, nrow=3, byrow = FALSE);
        Pxyz_c = readBin(fh, numeric(), n = 3, size = 4, endian = "big");

        D = diag(delta);
        Pcrs_c = c(ndim1/2, ndim2/2, ndim3/2);
        Pxyz_0 = Pxyz_c - ((Mdc %*% D) %*% Pcrs_c);

        blah = Mdc %*% D;
        M = matrix(rep(0, 16), nrow=4);
        M[1:3,1:3] = as.matrix(blah);
        M[4,1:4] = c(0,0,0,1);
        M[1:3,4] = Pxyz_0;

        ras_xform = matrix(rep(0, 16), nrow=4);
        ras_xform[1:3,1:3] = Mdc;
        ras_xform[4,1:4] = c(0,0,0,1);
        ras_xform[1:3,4] = Pxyz_c;

        header$internal$delta = delta;
        header$internal$Pxyz_c = Pxyz_c;
        header$internal$D = D;
        header$internal$Pcrs_c = Pcrs_c;
        header$internal$Pxyz_0 = Pxyz_0;
        header$internal$M = M;
        header$internal$Mdc = Mdc;

        header$vox2ras_matrix = as.matrix(M);
        header$ras_xform = ras_xform;

        RAS_space_size = (3*4 + 4*3*4);    # 60 bytes
        unused_header_space_size_left = unused_header_space_size_left - RAS_space_size;
    }

    # Skip to end of header/beginning of data
    seek(fh, where = unused_header_space_size_left - 2, origin = "current");

    nv = ndim1 * ndim2 * ndim3 * nframes;   # number of voxels
    volsz = c(ndim1, ndim2, ndim3, nframes);
    header$voldim_orig = volsz;
    header$voldim = volsz;   # May change later due to drop or flatten parameters

    # Determine size of voxel data, depending on dtype from header above
    MRI_UCHAR = 0L;
    MRI_INT = 1L;
    MRI_FLOAT = 3L;
    MRI_SHORT = 4L;

    dt_explanation = "0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT";

    # Determine number of bytes per voxel
    if(dtype == MRI_FLOAT) {
        nbytespervox = 4L;
        data = readBin(fh, numeric(), size = nbytespervox, n = nv, endian = "big");
    } else if(dtype == MRI_UCHAR) {
        nbytespervox = 1L;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, signed = FALSE, endian = "big");
    } else if (dtype == MRI_SHORT) {
        nbytespervox = 2L;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, endian = "big");
    } else if (dtype == MRI_INT) {
        nbytespervox = 4L;
        data = readBin(fh, integer(), size = nbytespervox, n = nv, endian = "big");
    } else {
       stop(sprintf(" ERROR: Unexpected data type found in header. Expected one of {0, 1, 3, 4} (%s) but got %d.\n", dt_explanation, dtype));
    }
    header$nbytespervox = nbytespervox;

    num_read = prod(length(data));
    if (num_read != nv) {
        stop(sprintf(" ERROR: read %d voxel values but expected to read %d.\n", num_read, nv));
    }

    # Reshape to expected dimensions
    data = array(data, dim = c(ndim1, ndim2, ndim3, nframes));

    if(flatten) {
        dim(data) = c(nv);
        data = as.vector(unlist(data));
        header$voldim = c(length(data));
    }

    header$has_mr_params = 0;
    if(with_header) {
        # Read the mr_params footer behind the data. The mr_params footer is optional, so we do not care if reading it fails.
        ignored = tryCatch({
            header$mr_params  = readBin(fh, numeric(), n = 4, size = 4, endian = "big");
            header$has_mr_params = 1;
        }, error=function(e){}, warning=function(w){});
    }

    close(fh);

    if(drop_empty_dims) {
      data = drop(data);
    }

    if(with_header) {
        return_list = list();
        return_list$header = header;
        return_list$data = data;
        return(return_list);
    }
    return(data);
}


#' @title Guess whether a file is gzipped.
#'
#' @description Guess whether a file is gzipped, based on the file extension.
#'
#' @param filepath string. Path to a file.
#'
#' @param gz_entensions list of strings. A list of suffixes that is considered indicative for the file being gzipped. Defaults to c(".gz", ".mgz"). Case does not matter.
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
            if(tolower(ext) == tolower(gz_ext)) {
                return(TRUE);
            }
        }
    }
    return(FALSE);
}
