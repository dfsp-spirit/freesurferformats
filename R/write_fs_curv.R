#' @title Write file in FreeSurfer curv format
#'
#' @description Write vertex-wise brain surface data to a file in FreeSurfer binary 'curv' format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.thickness', which contains n values. Each value represents the cortical thickness at the respective vertex in the brain surface mesh of bert.
#'
#' @param filepath, string. Full path to the output curv file. If it ends with ".gz", the file is written in gzipped format. Note that this is not common, and that other software may not handle this transparently.
#'
#' @param data vector of floats. The brain morphometry data to write, one value per vertex.
#'
#' @family morphometry functions
#'
#' @export
write.fs.curv <- function(filepath, data) {
    MAGIC_FILE_TYPE_NUMBER = 16777215;
    num_verts = length(data);
    num_faces = length(data);   # Has no meaning.
    values_per_vert = 1;

    if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
        fh = gzfile(filepath, "wb");
    } else {
        fh = file(filepath, "wb", blocking = TRUE);
    }

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


#' @title Write morphometry data in a format derived from the given file name (the file extension).
#'
#' @description Given data and a morphometry file name, derive the proper format and write the file.
#'
#' @param filepath, string. The full file name. The format to use will be derived from the last characters, the suffix. Supported suffixes are "mgh" for MGH format, "mgz" for MGZ format, everything else will be treated as curv format.
#'
#' @param data, numerical vector. The data to write.
#'
#' @return format, string. The format that was used to write the data. One of c("mgh", "mgz", "curv").
#'
#' @family morphometry functions
#'
#' @export
write.fs.morph <- function(filepath, data) {
    format = fs.get.morph.file.format.from.filename(filepath);
    if(format == "mgh" || format == "mgz" ) {
        write.fs.mgh(filepath, data);
    } else {
        write.fs.curv(filepath, data);
    }
    return(format);
}


#' @title Determine morphometry file format from filename
#'
#' @description Given a morphometry file name, derive the proper file format, based on the end of the string. Case is ignored, i.e., cast to lowercase before checks. If the filepath ends with "mgh", returns format "mgh". For suffix "mgz", returns "mgz" format. For all others, returns "curv" format.
#'
#' @param filepath, string. A path to a file.
#'
#' @return format, string. The format, one of c("mgz", "mgh", "curv").
#'
#' @family morphometry functions
#'
#' @export
fs.get.morph.file.format.from.filename <- function(filepath) {
    nc = nchar(filepath);
    num_chars_to_inspect = 3;
    if(nc >= num_chars_to_inspect) {
        ext = substr(filepath, nchar(filepath)-num_chars_to_inspect+1, nchar(filepath));
        if(tolower(ext) == "mgh") {
            return("mgh");
        }
        if(tolower(ext) == "mgz") {
            return("mgz");
        }
    }
    return("curv");
}



#' @title Determine morphometry file extension from format
#'
#' @description Given a morphometry file format, derive the proper file extension.
#'
#' @param format, string. One of c("mgh", "mgz", "curv").
#'
#' @return file ext, string. The standard file extension for the format. (May be an empty string for some formats.)
#'
#' @family morphometry functions
#'
#' @export
fs.get.morph.file.ext.for.format <- function(format) {
    if (format == "mgh") {
        return(".mgh");
    } else if(format == "mgz") {
        return(".mgz");
    } else if(format == "curv") {
        return("");
    } else {
        stop(sprintf("Unsupported morphometry file format: '%s'.", format));
    }
}

