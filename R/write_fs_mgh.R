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
        stop("The 'data' argument must be an array.");
    }
    if (!class(vox2ras_matrix)=="matrix") {
        stop("The 'vox2ras_matrix' argument must be a matrix.");
    }



    fh = file(filepath, "wb", blocking = TRUE);

    d = dim(data);
    num_dim = length(d);

    dim1 = d[1];
    dim2 = ifelse(num_dim >= 2, d[2], 1);
    dim3 = ifelse(num_dim >= 3, d[3], 1);
    num_frames = ifelse(num_dim >= 4, d[4], 1);

    writeBin(as.integer(1), fh, size = 4, endian = "big");
    writeBin(as.integer(dim1), fh, size = 4, endian = "big");
    writeBin(as.integer(dim2), fh, size = 4, endian = "big");
    writeBin(as.integer(dim3), fh, size = 4, endian = "big");
    writeBin(as.integer(num_frames), fh, size = 4, endian = "big");

    if (num_dim >= 5) {
        stop(sprintf("The data must not have more than 4 dimensions, but it has %d.", num_dim));
    }

    # write data type
    MRI_FLOAT = 3;
    writeBin(as.integer(MRI_FLOAT), fh, size = 4,  endian = "big");

    dof = 1;    # Unused, ignore
    writeBin(as.integer(dof), fh, size = 4, endian = "big");

    header_size_total = 256;    # MGH uses a fixed header size.


    MdcD = vox2ras_matrix[1:3, 1:3];   # The upper left 3x3 part of the 4x4 vox2ras matrix
    delta = sqrt(colSums(MdcD ** 2));    # a 3x1 vector

    delta_tvec = rep(delta, 3);  # 3x3 matrix
    Mdc = as.vector(MdcD / delta_tvec);
    Pcrs_c = c(dim1/2, dim2/2, dim3/2, 1);
    Pxyz_c = vox2ras_matrix * Pcrs_c;
    Pxyz_c = Pxyz_c[1:3];

    ras_flag = 1;
    writeBin(as.integer(ras_flag), fh, size = 2, endian = "big");
    if(ras_flag == 1) {
        writeBin(delta, fh, size = 4, endian = "big"); # 3x1 vector => 3x4 = 12 bytes
        writeBin(Mdc, fh, size = 4, endian = "big");  # 3x3matrix => 9x4 = 36 bytes
        writeBin(Pxyz_c, fh, size = 4, endian = "big"); # 3x1 matrix => 3x4 = 12 bytes
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
