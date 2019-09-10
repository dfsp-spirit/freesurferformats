#' @title Write file in FreeSurfer MGH format
#'
#' @description Write brain data to a file in FreeSurfer binary MGH format.
#'
#' @param data, matrix of numerical values. The brain data to write.
#'
#' @param filepath, string. Full path to the output curv file.
#'
#' @param vox2ras_matrix, 4x4 matrix. An affine transformation matrix for the RAS transform that maps voxel indices in the volume to coordinates, such that for y(i1,i2,i3) (i.e., a voxel defined by 3 indices in the volume), the xyz coordinates are vox2ras_matrix*[i1 i2 i3 1].
#'
#' @param mr_params, numerical vector of length four. The acquisition parameters, in order: tr, flipangle, te, ti. Defaults to c(0, 0, 0, 0) if omitted.
#'
#'
#'
#' @export
write.fs.mgh <- function(filepath, data, vox2ras_matrix = matrix(c(1,0,0,0, 0,1,0,0, 0,0,1,0, 1,1,1,1), nrow=4), mr_params = c(0, 0, 0, 0)) {

    # Sanity checks for arguments
    if (!class(data)=="array") {
        stop("The 'data' argument must be an array");
    }
    if (!class(vox2ras_matrix)=="matrix") {
        stop("The 'vox2ras_matrix' argument must be a matrix");
    }

    MRI_FLOAT = 3;
    MRI_TENSOR = 6;

    fh = file(filepath, "wb", blocking = TRUE);

    d = dim(data);
    num_dim = length(d);
    cat(sprintf("Received data with %d dimensions: %s.\n", num_dim, paste(d, collapse = ' ')));

    dim1 = d[1];
    dim2 = ifelse(num_dim >= 2, d[2], 1);
    dim3 = ifelse(num_dim >= 3, d[3], 1);
    num_frames = ifelse(num_dim >= 4, d[4], 1);
    cat(sprintf("Writing data with dimensions: %d, %d, %d, %d.\n", dim1, dim2, dim3, num_frames));

    writeBin(as.integer(1), fh, size = 4, endian = "big");
    writeBin(as.integer(dim1), fh, size = 4, endian = "big");
    writeBin(as.integer(dim2), fh, size = 4, endian = "big");
    writeBin(as.integer(dim3), fh, size = 4, endian = "big");
    writeBin(as.integer(num_frames), fh, size = 4, endian = "big");

    if (num_dim == 5) {
        is_tensor = TRUE;
        writeBin(as.integer(MRI_TENSOR), fh, size = 4, endian = "big");
    } else {
        is_tensor = FALSE;
        writeBin(as.integer(MRI_FLOAT), fh, size = 4,  endian = "big");
    }

    dof = 1;    # Unused, ignore
    writeBin(as.integer(dof), fh, size = 4, endian = "big");

    header_size_total = 256;


    MdcD = vox2ras_matrix[1:3, 1:3];

    delta = sqrt(sum(MdcD ** 2));
    cat(sprintf("delta is %f\n", delta));

    delta_tvec = matrix(rep(delta, 3));
    delta_tvec = rep(delta, 3);
    print("delta_tvec:", delta_tvec)
    print("Computing Mdc")
    Mdc = as.vector(MdcD / delta_tvec);
    cat(sprintf("Mdc=%f.\n", Mdc));
    Pcrs_c = c(dim1/2, dim2/2, dim3/2, 1);
    Pxyz_c = vox2ras_matrix * Pcrs_c;
    Pxyz_c = Pxyz_c[1:3];

    cat(sprintf("Writing RAS information.\n"));
    # Write RAS-good flag as a short
    ras_flag = 0;
    writeBin(as.integer(ras_flag), fh, size = 2, endian = "big");
    if(ras_flag == 1) {
        writeBin(delta, fh, size = 4, endian = "big");
        writeBin(Mdc, fh, size = 4, endian = "big");
        writeBin(Pxyz_c, fh, size = 4, endian = "big");
        used_space_RAS = (3*4 + 4*3*4);
    } else {
        used_space_RAS = 0;
    }

    # The header has a fixed size (data starts at a fixed index), so fill the rest with zeros.
    space_to_fill = header_size_total - used_space_RAS;
    writeBin(as.integer(rep.int(0, space_to_fill)), fh, size = 1, endian = "big");

    # Write the data:
    writeBin(as.numeric(as.vector(data)), fh, size = 4, endian = "big");

    # A footer follows the data, it contains the MR acquisition parameters
    writeBin(mr_params, fh, endian = "big");
    close(fh);
}
