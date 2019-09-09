#' @title Write file in FreeSurfer MGH format
#'
#' @description Write brain data to a file in FreeSurfer binary MGH format.
#'
#' @param data, matrix of numerical values. The brain data to write.
#'
#' @param filepath, string. Full path to the output curv file.
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
