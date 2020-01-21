#' @title Read file in FreeSurfer curv format
#'
#' @description Read vertex-wise brain morphometry data from a file in FreeSurfer 'curv' format. Bboth binary and ASCII versions are supported.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.thickness', which contains n values. Each value represents the cortical thickness at the respective vertex in the brain surface mesh of bert.
#'
#' @param filepath string. Full path to the input curv file. Note: gzipped binary curv files are supported and gz binary format is assumed if the filepath ends with ".gz".
#'
#' @param format one of 'auto', 'asc', or 'bin'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.asc'.
#'
#' @return data vector of floats. The brain morphometry data, one value per vertex.
#'
#' @examples
#'     curvfile = system.file("extdata", "lh.thickness",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     ct = read.fs.curv(curvfile);
#'     cat(sprintf("Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
#'                             length(ct), min(ct), mean(ct), max(ct)));
#'
#' @family morphometry functions
#'
#' @export
read.fs.curv <- function(filepath, format='auto') {
    MAGIC_FILE_TYPE_NUMBER = 16777215;

    if(!(format %in% c('auto', 'bin', 'asc'))) {
      stop("Format must be one of c('auto', 'bin', 'asc').");
    }

    if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
      return(read.fs.curv.asc(filepath));
    }

    if(guess.filename.is.gzipped(filepath)) {
        fh = gzfile(filepath, "rb");
    } else {
        fh = file(filepath, "rb");
    }
    on.exit({ close(fh) }, add=TRUE);

    magic_byte = fread3(fh);
    if (magic_byte != MAGIC_FILE_TYPE_NUMBER) {
        stop(sprintf("Magic number mismatch (%d != %d). The given file '%s' is not a valid FreeSurfer 'curv' format file in new binary format. (Hint: This function is designed to read files like 'lh.area' in the 'surf' directory of a pre-processed FreeSurfer subject.)\n", magic_byte, MAGIC_FILE_TYPE_NUMBER, filepath));
    }
    num_verts = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    num_faces = readBin(fh, integer(), n = 1, size = 4, endian = "big");
    values_per_vertex = readBin(fh, integer(), n = 1, endian = "big");
    data = readBin(fh, numeric(), size = 4, n = num_verts, endian = "big");
    return(data);
}


#' @title Read morphometry data from ASCII curv format file
#'
#' @param filepath path to a file in FreeSurfer ASCII curv format. Such a file contains, on each line, the following fields, separated by spaces: vertex_index, vertex_coord_x,  vertex_coord_y,  vertex_coord_z,  morph_data_value.
#'
#' @return numeric vector, the curv data
#'
#' @keywords internal
read.fs.curv.asc <- function(filepath) {
  curv_df = read.table(filepath, header=FALSE, col.names=c("vert_index", "coord_x", "coord_y", "coord_z", "morph_data"), colClasses = c("integer", "numeric", "numeric", "numeric", "numeric"));
  return(curv_df$morph_data);
}


#' @title Read 3-byte integer.
#'
#' @description Read a 3-byte integer from a binary file handle. Advances the pointer accordingly.
#'
#' @param filehandle: file handle
#'
#' @return integer: The read integer.
#'
#'
#' @keywords internal
fread3 <- function(filehandle) {
    b1 = readBin(filehandle, integer(), size=1, signed = FALSE);
    b2 = readBin(filehandle, integer(), size=1, signed = FALSE);
    b3 = readBin(filehandle, integer(), size=1, signed = FALSE);
    res = bitwShiftL(b1, 16) + bitwShiftL(b2, 8) + b3;
    return(res);
}


#' @title Read morphometry data file in any FreeSurfer format.
#'
#' @description Read vertex-wise brain surface data from a file. The file can be in any of the supported formats, and the format will be determined from the file extension.
#'
#' @param filepath, string. Full path to the input file. The suffix determines the expected format as follows: ".mgz" and ".mgh" will be read with the read.fs.mgh function, all other file extensions will be read with the read.fs.curv function.
#'
#' @return data, vector of floats. The brain morphometry data, one value per vertex.
#'
#' @examples
#'     curvfile = system.file("extdata", "lh.thickness",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     ct = read.fs.morph(curvfile);
#'     cat(sprintf("Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
#'                             length(ct), min(ct), mean(ct), max(ct)));
#'
#'
#'     mghfile = system.file("extdata", "lh.curv.fwhm10.fsaverage.mgz",
#'                             package = "freesurferformats", mustWork = TRUE);
#'     curv = read.fs.morph(mghfile);
#'     cat(sprintf("Read data for %d vertices. Values: min=%f, mean=%f, max=%f.\n",
#'                             length(ct), min(ct), mean(ct), max(ct)));
#'
#' @family morphometry functions
#'
#' @export
read.fs.morph <- function(filepath) {
    format = fs.get.morph.file.format.from.filename(filepath);
    return_list = list();
    if(format == "mgh" || format=="mgz") {
        data = read.fs.mgh(filepath, flatten=TRUE);
    }
    else {
        data = read.fs.curv(filepath);
    }
    return(data);
}
