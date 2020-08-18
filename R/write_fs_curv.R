#' @title Write file in FreeSurfer curv format
#'
#' @description Write vertex-wise brain surface data to a file in FreeSurfer binary 'curv' format.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.thickness', which contains n values. Each value represents the cortical thickness at the respective vertex in the brain surface mesh of bert.
#'
#' @param filepath, string. Full path to the output curv file. If it ends with ".gz", the file is written in gzipped format. Note that this is not common, and that other software may not handle this transparently.
#'
#' @param data vector of doubles. The brain morphometry data to write, one value per vertex.
#'
#' @family morphometry functions
#'
#' @export
write.fs.curv <- function(filepath, data) {
    if(! is.double(data)) {
      warning("Parameter 'data' is not of type double, trying to coerce.");
      data = as.double(data);
    }
    MAGIC_FILE_TYPE_NUMBER = 16777215;
    num_verts = length(data);
    num_faces = length(data);   # Has no meaning.
    values_per_vert = 1L;

    if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
        fh = gzfile(filepath, "wb");
    } else {
        fh = file(filepath, "wb", blocking = TRUE);
    }

    fwrite3(fh, MAGIC_FILE_TYPE_NUMBER);
    writeBin(as.integer(num_verts), fh, endian = "big");
    writeBin(as.integer(num_faces), fh, endian = "big");
    writeBin(as.integer(values_per_vert), fh, endian = "big");
    writeBin(data, fh, size = 4L, endian = "big");
    close(fh);
}


#' @title Write file in FreeSurfer ASCII curv format
#'
#' @description Write vertex-wise brain surface data to a file in FreeSurfer ascii 'curv' format.
#'
#' @inheritParams write.fs.curv
#'
#' @param coords optional, nx3 matrix of x,y,z coordinates, one row per vertex in 'data'. If `NULL`, all zeroes will be written instead.
#'
#' @family morphometry functions
#'
#' @export
write.fs.morph.asc <- function(filepath, data, coords = NULL) {
  if(! is.double(data)) {
    warning("Parameter 'data' is not of type double, trying to coerce.");
    data = as.double(data);
  }
  num_verts = length(data);
  if(is.null(coords)) {
    coords = matrix(rep(0.0, (num_verts * 3)), ncol = 3L);
  }
  if(nrow(coords) != length(data)) {
    stop(sprintf("If 'coords' is given, its number of rows (%d) must match the length of the data parameter (%d).\n", ncol(coords), length(data)));
  }
  if(ncol(coords) != 3L) {
    stop("Matrix 'coords' must have exactly 3 columns (x,y,z).");
  }
  cx = coords[,1];
  cy = coords[,2];
  cz = coords[,3];
  df = data.frame('vert_index'=seq.int(0, (num_verts - 1L)), 'coord_x'=cx, 'coord_y'=cy, 'coord_z'=cz, 'morph_data'=data);
  write.table(df, file = filepath, quote = FALSE, row.names = FALSE, col.names = FALSE);
}


#' @title Write curv data to file in simple text format
#'
#' @description Write vertex-wise brain surface data to a file in a simple text format: one value per line.
#'
#' @inheritParams write.fs.curv
#'
#' @family morphometry functions
#'
#' @export
write.fs.morph.txt <- function(filepath, data) {
  if(! is.double(data)) {
    warning("Parameter 'data' is not of type double, trying to coerce.");
    data = as.double(data);
  }
  write.table(data, file = filepath, quote = FALSE, row.names = FALSE, col.names = FALSE);
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


#' @title Write morphometry data in a format derived from the given file name.
#'
#' @description Given data and a morphometry file name, derive the proper format from the file extension and write the file.
#'
#' @param filepath, string. The full file name. The format to use will be derived from the last characters, the suffix. Supported suffixes are "mgh" for MGH format, "mgz" for MGZ format, everything else will be treated as curv format.
#'
#' @param data, numerical vector. The data to write.
#'
#' @param format character string, the format to use. One of c("auto", "mgh", "mgz", "curv"). The default setting "auto" will determine the format from the file extension.
#'
#' @param ... additional parameters to pass to \code{\link[freesurferformats]{write.fs.mgh}}. Only applicable for MGH and MGZ format output files, ignored for curv files.
#'
#' @return format, string. The format that was used to write the data. One of c("auto", "mgh", "mgz", "curv", "gii").
#'
#' @family morphometry functions
#'
#' @export
write.fs.morph <- function(filepath, data, format='auto', ...) {

    if(! format %in% c("auto", "mgh", "mgz", "curv", "gii", "smp")) {
      stop("Format must be one of 'auto', 'mgh', 'mgz', 'curv', 'smp', or 'gii'.");
    }

    if(format == 'auto') {
      format = fs.get.morph.file.format.from.filename(filepath);
    }
    if(format == "mgh" || format == "mgz" ) {
        write.fs.mgh(filepath, data, ...);
    } else if (format == "gii") {
        write.fs.morph.gii(filepath, data);
    } else if (format == "curv") {
        write.fs.curv(filepath, data);
    }
    return(invisible(format));
}


#' @title Write morphometry data in GIFTI format.
#'
#' @description The data will be written with intent 'NIFTI_INTENT_SHAPE' and as datatype 'NIFTI_TYPE_FLOAT32'.
#'
#' @param filepath string, the full path of the output GIFTI file.
#'
#' @param data numerical vector, the data to write. Will be coerced to double.
#'
#' @return format, string. The format that was used to write the data: "gii".
#'
#' @family morphometry functions
#' @family gifti writers
#'
#' @export
write.fs.morph.gii <- function(filepath, data) {
  data = as.double(data);
  gifti_writer(filepath, list(data), intent='NIFTI_INTENT_SHAPE', datatype='NIFTI_TYPE_FLOAT32');
  return(invisible('gii'));
}


#' @title Determine morphometry file format from filename
#'
#' @description Given a morphometry file name, derive the proper file format, based on the end of the string. Case is ignored, i.e., cast to lowercase before checks. If the filepath ends with "mgh", returns format "mgh". For suffix "mgz", returns "mgz" format. For all others, returns "curv" format.
#'
#' @param filepath, string. A path to a file.
#'
#' @return format, string. The format, one of c("mgz", "mgh", "curv", "gii").
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
        if(tolower(ext) == "gii") {
          return("gii");
        }
        if(tolower(ext) == ".gz") {
            # Check whether it is '.gii.gz'
            num_chars_to_inspect_deep = 7;
            if(nc >= num_chars_to_inspect_deep) {
                deep_ext = substr(filepath, nchar(filepath)-num_chars_to_inspect_deep+1, nchar(filepath));
            }
            if(tolower(deep_ext) == ".gii.gz") {
                return("gii"); # The gifti reader function handles gii.gz.
            }
            # Otherwise we assume gzipped curv format.
        }
    }
    return("curv");
}



#' @title Determine morphometry file extension from format
#'
#' @description Given a morphometry file format, derive the proper file extension.
#'
#' @param format, string. One of c("mgh", "mgz", "curv", "gii").
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
    } else if(format == "gii") {
      return(".gii");
    } else {
        stop(sprintf("Unsupported morphometry file format: '%s'.", format));
    }
}


#' @title Write morphometry data in Brainvoyager SMP format.
#'
#' @param filepath string, the full path of the output SMP file.
#'
#' @param data numerical vector, the data to write. Will be coerced to double.
#'
#' @param ... extra arguments passed to \code{\link{write.smp.brainvoyager}}. Allows yout to save in specific format versions.
#'
#' @return format, string. The format that was used to write the data.
#'
#' @family morphometry functions
#'
#' @export
write.fs.morph.smp <- function(filepath, data, ...) {
  write.smp.brainvoyager(filepath, bvsmp(data), ...);
  return("smp");
}

