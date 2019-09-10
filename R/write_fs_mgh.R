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
#' @param mr_parms, numerical vector of length four. The acquisition parameters, in order: tr, flipangle, te, ti. Defaults to c(0, 0, 0, 0) if omitted.
#'
#'
#'
#' @export
write.fs.mgh <- function(filepath, data, vox2ras_matrix, mri_params = c(0, 0, 0, 0)) {

    fh = file(filepath, "wb", blocking = TRUE);

    d = dim(data);
    dim1 =
    dim2 =
    dim3 =

    num_frames = 1;

    writeBin(as.integer(1), fh, endian = "big");

    writeBin(as.integer(dim1), fh, endian = "big");
    writeBin(as.integer(dim2), fh, endian = "big");
    writeBin(as.integer(dim3), fh, endian = "big");
    writeBin(as.integer(num_frames), fh, endian = "big");




    close(fh);
}
