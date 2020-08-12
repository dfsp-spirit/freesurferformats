#' @title Read file in FreeSurfer curv format
#'
#' @description Read vertex-wise brain morphometry data from a file in FreeSurfer 'curv' format. Both the binary and ASCII versions are supported.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/surf/lh.thickness', which contains n values. Each value represents the cortical thickness at the respective vertex in the brain surface mesh of bert.
#'
#' @param filepath string. Full path to the input curv file. Note: gzipped binary curv files are supported and gz binary format is assumed if the filepath ends with ".gz".
#'
#' @param format one of 'auto', 'asc', 'bin', or 'txt'. The format to assume. If set to 'auto' (the default), binary format will be used unless the filepath ends with '.asc' or '.txt'. The latter is just one float value per line in a text file.
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
    MAGIC_FILE_TYPE_NUMBER = 16777215L;

    if(!(format %in% c('auto', 'bin', 'asc', 'txt'))) {
      stop("Format must be one of c('auto', 'bin', 'asc', 'txt').");
    }

    if(format == 'asc' | (format == 'auto' & filepath.ends.with(filepath, c('.asc')))) {
      return(read.fs.morph.asc(filepath));
    }

    if(format == 'txt' | (format == 'auto' & filepath.ends.with(filepath, c('.txt')))) {
      return(read.fs.morph.txt(filepath));
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
#' @note This format is also known as *dpv* (data-per-vertex) format.
#'
#' @export
read.fs.morph.asc <- function(filepath) {
  curv_df = read.table(filepath, header=FALSE, col.names=c("vert_index", "coord_x", "coord_y", "coord_z", "morph_data"), colClasses = c("integer", "numeric", "numeric", "numeric", "numeric"));
  return(curv_df$morph_data);
}


#' @title Read morphometry data from plain text file
#'
#' @param filepath path to a file in plain text format. Such a file contains, on each line, a single float value. This very simply and limited *format* is used by the LGI tool by Lyu et al., and easy to generate in shell scripts.
#'
#' @return numeric vector, the curv data
#'
#' @export
read.fs.morph.txt <- function(filepath) {
  curv_df = read.table(filepath, header=FALSE, col.names=c("morph_data"), colClasses = c("numeric"));
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
#' @param format character string, the format to use. One of c("auto", "mgh", "mgz", "curv", "gii"). The default setting "auto" will determine the format from the file extension.
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
read.fs.morph <- function(filepath, format='auto') {
    if(! format %in% c("auto", "mgh", "mgz", "curv", "gii")) {
        stop("Format must be one of 'auto', 'mgh', 'mgz', 'curv', or 'gii'.");
    }

    if(format == 'auto') {
        format = fs.get.morph.file.format.from.filename(filepath);
    }

    if(format == "mgh" || format=="mgz") {
        data = read.fs.mgh(filepath, flatten=TRUE);
    } else if(format == "gii") {
        data = read.fs.morph.gii(filepath);
    } else {
        data = read.fs.curv(filepath);
    }
    return(data);
}


#' @title Read morphometry data file in GIFTI format.
#'
#' @description Read vertex-wise brain surface data from a GIFTI file. The file must be a GIFTI *func* file (not a GIFTI *surf* file containing a mesh, use \code{\link[freesurferformats]{read_nisurface}} for loading GIFTI surf files).
#'
#' @param filepath, string. Full path to the input GIFTI file.
#'
#' @param element_index integer, the element to load in case the GIFTI file containes several datasets (usually time series). Defaults to the first element, 1L.
#'
#' @return data, vector of double or integer. The brain morphometry data, one value per vertex. The data type depends on the data type in the file.
#'
#' @note This function requires the `gifti` package, which is an optional dependency, to be installed. It also assumes that the dataset contains a vector or a matrix/array in which all dimensions except for 1 are empty.
#'
#' @family morphometry functions
#' @family gifti readers
#'
#' @export
read.fs.morph.gii <- function(filepath, element_index=1L) {
  if(element_index < 1L) {
    stop("Parameter 'element_index' must be a positive integer.");
  }
  if (requireNamespace("gifti", quietly = TRUE)) {
      # Try to read via gifti package
      gii = gifti::read_gifti(filepath);
      if(element_index > length(gii$data)) {
        stop(sprintf("Requested data element at index '%d', but GIFTI file contains %d elements only.\n", element_index, length(gii$data)));
      }
      # Data may be stored in a matrix or higher dim array (with empty dimensions in case of vertex-wise data). Drop the empty dims to get a vector.
      morph_data = drop(gii$data[[element_index]]);
      if(! is.null(dim(morph_data))) {
        stop("Dropping empty dimensions of the GIFTI data did not result in a vector. The data in the file cannot be interpreted as scalar per-vertex data.");
      }
      return(morph_data);
  } else {
    stop("Reading files in GIFTI format requires the 'gifti' package to be installed.");
  }
}

#' @title Read Brainvoyager vertex-wise statistical surface data from SMP file.
#'
#' @param filepath character string, path to file in Brainvoyager SMP file format
#'
#' @param map_index positive integer or character string, the surface value map to load (an SMP file can contain several values per vertex, i.e., several surface maps). If an integer, interpreted as the index of the map. If a character string, as the name of the map.
#'
#' @return numeric vector, the values from the respective map.
#'
#' @export
read.fs.morph.bvsmp <- function(filepath, map_index = 1L) {
  smp = read.smp.brainvoyager(filepath);
  if(is.integer(map_index)) {
    if(map_index > smp$num_maps) {
      stop(sprintf("Requested SMP statistical map # %d, but file contains only %d maps.\n", map_index, smp$num_maps));
    }
    return(smp$vertex_maps[[map_index]]$data);
  } else {
    map_name = map_index;
    available_maps = c();
    for(mi in seq.int(smp$num_maps)) {
      if(smp$vertex_maps[[mi]]$name == map_name) {
        return(smp$vertex_maps[[mi]]$data);
        available_maps = c(available_maps, smp$vertex_maps[[mi]]$name);
      }
    }
    stop(sprintf("Requested map not found, available maps: %s \n", paste(available_maps, collapse = ", ")));
  }
}


#' @title Read Brainvoyager statistical surface results from SMP file.
#'
#' @param filepath character string, path to file in Brainvoyager SMP file format
#'
#' @references see \url{https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/40-the-format-of-smp-files} for the spec
#'
#' @note Currently only SMP file version 3 is supported.
#'
#' @return named list of file contents
#' @export
read.smp.brainvoyager <- function(filepath) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);

  if(ret_list$smp_version != 3L) {
    stop(sprintf("Found SMP file in file format version %d, only version 3 is supported.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  for(map_index in seq.int(ret_list$num_maps)) {
    vm = list();
    vm$map_type = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$num_lags = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$min_lag = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$max_lag = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cc_overlay = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cluster_size = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$enable_cluster_check = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$stat_threshold_critical = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_1_fnom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_2_fdenom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cortex_bonferroni_correct = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$color_critical_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$color_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$enable_smp_color = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$transparent_color_factor = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$map_name = readBin(fh, character(), n = 1);
    ret_list$vertex_maps[[map_index]] = vm;
  }

  # header done, now read the data
  for(map_index in seq.int(ret_list$num_maps)) {
    ret_list$vertex_maps[[map_index]]$data = readBin(fh, numeric(), size = 4, n = ret_list$num_mesh_vertices, endian = endian);
  }
  return(ret_list);
}
