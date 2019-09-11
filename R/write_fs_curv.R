#' @title Write file in FreeSurfer curv format
#'
#' @description Write vertex-wise brain surface data to a file in FreeSurfer binary 'curv' format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.thickness', which contains n values. Each value represents the cortical thickness at the respective vertex in the brain surface mesh of bert.
#'
#' @param filepath, string. Full path to the output curv file.
#'
#' @param data, vector of floats. The brain morphometry data to write, one value per vertex.
#'
#'
#' @export
write.fs.curv <- function(filepath, data) {
    MAGIC_FILE_TYPE_NUMBER = 16777215;
    num_verts = length(data);
    num_faces = length(data);   # Has no meaning.
    values_per_vert = 1;

    fh = file(filepath, "wb", blocking = TRUE);
    fwrite3(fh, MAGIC_FILE_TYPE_NUMBER);
    writeBin(as.integer(num_verts), fh, endian = "big");
    writeBin(as.integer(num_faces), fh, endian = "big");
    writeBin(as.integer(values_per_vert), fh, endian = "big");
    writeBin(data, fh, size = 4, endian = "big");
    close(fh);
}


#' @title Write 3-byte integer.
#'
#' @description Write a 3-byte integer to a binary file handle.
#'
#' @param filehandle: file handle
#'
#' @param data: number to write
#'
#'
#' @keywords internal
fwrite3 <- function(filehandle, data) {
    b1 = bitwAnd(bitwShiftR(data, 16), 255);
    b2 = bitwAnd(bitwShiftR(data, 8), 255);
    b3 = bitwAnd(data, 255);
    writeBin(b1, filehandle, size = 1, endian = "big");
    writeBin(b2, filehandle, size = 1, endian = "big");
    writeBin(b3, filehandle, size = 1, endian = "big");
}


write.fs.morph <- function(filepath, data) {
    nc = nchar(filepath);
    num_chars_to_inspect = 3;
    if(nc >= num_chars_to_inspect) {
        ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1, nchar(filepath));
        if(tolower(ext) == "mgh") {
            write.fs.mgh(filepath, data);
            return("mgh");
        }
        if(tolower(ext) == "mgz") {
            write.fs.mgh(filepath, data, gzipped=TRUE);
            return("mgz");
        }
    }
    write.fs.curv(filepath, data);
    return("curv");
}

