#' @title Read file in FreeSurfer label format
#'
#' @description Read a mask in FreeSurfer label format. A label defines a list of vertices (of an associated surface or morphometry file) which are part of it. All others are not. You can think of it as binary mask. Label files are ASCII text files, which have 5 columns (vertex index, coord1, coord2, coord3, value), but only the vertex indices are of interest. A label can also contain voxels, in that case the indices are -1 and the coordinates are important.
#'
#' @param filepath string. Full path to the input label file.
#'
#' @param return_one_based_indices logical. Whether the indices should be 1-based. Indices are stored zero-based in the file, but R uses 1-based indices. Defaults to TRUE, which means that 1 will be added to all indices read from the file before returning them. Notice that for volume labels, the indices are negative (-1), and the coord fields contain the *positions* of the voxels it tkras space (**not** the voxel *indices* in a volume). If a file contains negative indices, they will NOT be incremented, no matter what this is set to.
#'
#' @param full logical, whether to return a full object of class `fs.label` instead of only a vector containing the vertex indices. If TRUE, a named list with the following two entries is returned: 'one_based_indices': logical, whether the vertex indices are one-based. 'vertexdata': a data.frame with the following columns: 'vertex_index': integer, see parameter 'return_one_based_indices', 'coord1', 'coord2', 'coord3': float coordinates, 'value': float, scalar data for the vertex, can mean anything. This parameter defaults to FALSE.
#'
#' @param metadata named list of arbitrary metadata to store in the instance, ignored unless the paramter `full` is TRUE.
#'
#' @return vector of integers or `fs.label` instance (see parameter `full`). The vertex indices from the label file. See the parameter `return_one_based_indices` for important information regarding the start index.
#'
#' @family label functions
#'
#' @note To load volume/voxel labels, you will have to set the 'full' parameter to `TRUE`.
#'
#' @examples
#'     labelfile = system.file("extdata", "lh.entorhinal_exvivo.label",
#'       package = "freesurferformats", mustWork = TRUE);
#'     label = read.fs.label(labelfile);
#'
#' @export
#' @importFrom utils read.table
read.fs.label <- function(filepath, return_one_based_indices=TRUE, full=FALSE, metadata=list()) {

    # The first line is a comment, and the 2nd one contains a single number: the number of vertex lines following.
    num_verts_df = read.table(filepath, skip=1L, nrows=1L, col.names = c('num_verts'), colClasses = c("integer"));
    num_verts = num_verts_df$num_verts[1];

    vertices_df = read.table(filepath, skip=2L, col.names = c('vertex_index', 'coord1', 'coord2', 'coord3', 'value'), colClasses = c("integer", "numeric", "numeric", "numeric", "numeric"));
    vertices = vertices_df$vertex_index;

    if(length(vertices) != num_verts) {
      stop(sprintf("Expected %d vertex rows in label file '%s' from header, but received %d.\n", num_verts, filepath, length(vertices)));
    }

    if(any(vertices < 0L)) {
      label_type = 'volume_label';
    } else {
      label_type = 'surface_label';
    }

    if(return_one_based_indices) {
      if(label_type == 'surface_label') {
        vertices = vertices + 1L;
        vertices_df$vertex_index = vertices;
      }

    }
    if(full) {
      ret_list = list("vertexdata"=vertices_df, "metadata"=metadata);
      if(return_one_based_indices) {
        ret_list$one_based_indices = TRUE;
      } else {
        ret_list$one_based_indices = FALSE;
      }

      ret_list$label_type = label_type;

      class(ret_list) = c('fs.label', class(ret_list));
      return(ret_list);
    } else {
      return(vertices);
    }
}


#' @title Print description of a brain surface label.
#'
#' @param x brain surface label with class `fs.label`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.label <- function(x, ...) {
  if(nrow(x$vertexdata) > 0L) {
    vertex_data_range = range(x$vertexdata$value);

    if(any(x$vertexdata$vertex_index < 0L)) {
      # It's a volume label (not a surface label).
      cat(sprintf("Brain volume label containing %d voxels, values are in range (%.3f, %.3f). Summary:\n", nrow(x$vertexdata), vertex_data_range[1], vertex_data_range[2]));
      print(summary(x$vertexdata$value));
    } else {
      cat(sprintf("Brain surface label containing %d vertices, values are in range (%.3f, %.3f). Summary:\n", nrow(x$vertexdata), vertex_data_range[1], vertex_data_range[2]));
      print(summary(x$vertexdata$value));
      if(x$one_based_indices) {
        cat(sprintf("Vertex indices start at: 1\n"));
      } else {
        cat(sprintf("Vertex indices start at: 0\n"));
      }
    }

    cat(sprintf("Label coordinates: minimal values are (%.2f, %.2f, %.2f), maximal values are (%.2f, %.2f, %.2f).\n", min(x$vertexdata$coord1), min(x$vertexdata$coord2), min(x$vertexdata$coord3), max(x$vertexdata$coord1), max(x$vertexdata$coord2), max(x$vertexdata$coord3)));
  } else {
    cat(sprintf("Brain label containing %d entries.\n", nrow(x$vertexdata)));
  }
}


#' @title Check whether object is an fs.label
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain surface label (that is, has `fs.label` amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.label <- function(x) inherits(x, "fs.label")



#' @title Read a label from a GIFTI label/annotation file.
#'
#' @param filepath string. Full path to the input label file.
#'
#' @param label_value integer, the label value of interest to extract from the annotation.
#'
#' @param element_index positive integer, the index of the dataarray to return. Ignored unless the file contains several dataarrays.
#'
#' @return integer vector,  the vertex indices of the label
#'
#' @note A gifti label is more like a FreeSurfer annotation, as it assigns a label integer (region code) to each vertex instead of listing a set of vertex indices. It is recommended to read it with \code{\link[freesurferformats]{read.fs.annot.gii}} instead. This function extracts one of the regions from the annotation as a label.
#'
#' @family label functions
#'
#' @export
read.fs.label.gii <- function(filepath, label_value, element_index=1L) {

  if( ! is.integer(label_value)) {
    if(is.numeric(label_value) | is.logical(label_value)) {
      label_value = as.integer(label_value);
    } else {
      stop("Parameter 'label_value' must be an integer, like 1L.");
    }
  }

  if (requireNamespace("gifti", quietly = TRUE)) {
    gii = gifti::read_gifti(filepath);
    intent = gii$data_info$Intent[[element_index]];
    if(intent != 'NIFTI_INTENT_LABEL') {
      warning(sprintf("The intent of the gifti file is '%s', expected 'NIFTI_INTENT_LABEL'.\n", intent));
    }
    if(is.null(gii$label)) {
      stop(sprintf("The gifti file '%s' does not contain label information.\n", filepath));
    } else {

      label_data_num_columns = ncol(gii$data[[element_index]]); # must be 1D for surface labels: 1 column of vertex indices (the data is returned as a matrix).
      if(gii$data_info$Dimensionality != 1L) {
        stop(sprintf("Label data has %d dimensions, expected 1. This does not look like a 1D surface label.\n", gii$data_info$Dimensionality));
      }

      annot_data = as.integer(gii$data[[element_index]]); # note that as.integer() turns the (1 column) matrix into a vector.

      # Note: gifti labels seem to be more like a mask or an annotation: they assign a value to each vertex of the surface instead of listing
      # all vertices which are part of the label. Reading them as a label in the FreeSurfer sense potentially means losing
      # information (if they contain more than 2 region types). If they only contain positive/negative labels, it is fine.
      num_regions_in_annot = nrow(gii$label);
      return(which(annot_data == label_value));
    }

  } else {
    stop("The 'gifti' package must be installed to use this functionality.");
  }

}


#' @title Read an annotation or label in GIFTI format.
#'
#' @param filepath string. Full path to the input label file.
#'
#' @param element_index positive integer, the index of the dataarray to return. Ignored unless the file contains several dataarrays.
#'
#' @param data_only logical, whether to ignore the colortable and region names, and only return the raw vector that contains one integer label per vertex.
#'
#' @param rgb_column_names vector of exactly 4 character strings, order is important. The column names for the red, green, blue and alpha channels in the lable table. If a column does not exist, pass NA. If you do not know the column names, just call the function, it will print them. See 'data_only' if you do not care.
#'
#' @param key_column_name character string, the column name for the key column in the lable table. This is the column that holds the label value from the raw vector (see 'data_only') that links a label value to a row in the label table. Without it, one cannot recostruct the region name and color of an entry. Passing NA has the same effect as setting 'data_only' to TRUE.
#'
#' @export
#' @importFrom xml2 xml_find_all read_xml xml_text
read.fs.annot.gii <- function(filepath, element_index=1L, data_only=FALSE, rgb_column_names = c('Red', 'Green', 'Blue', 'Alpha'), key_column_name = 'Key') {

  if(length(rgb_column_names) != 4L) {
    stop("Parameter 'rgb_column_names' must have length 4. Hint: Pass NA for values which are not available.");
  }

  if (requireNamespace("gifti", quietly = TRUE)) {
    gii = gifti::read_gifti(filepath);
    intent = gii$data_info$Intent[[element_index]];
    if(intent != 'NIFTI_INTENT_LABEL') {
      warning(sprintf("The intent of the gifti file is '%s', expected 'NIFTI_INTENT_LABEL'.\n", intent));
    }
    if(is.null(gii$label)) {
      stop(sprintf("The gifti file '%s' does not contain label information.\n", filepath));
    } else {

      label_data_num_columns = ncol(gii$data[[element_index]]); # must be 1D for surface labels: 1 column of vertex indices (the data is returned as a matrix).
      if(gii$data_info$Dimensionality != 1L) {
        stop(sprintf("Label data has %d dimensions, expected 1. This does not look like a 1D surface label.\n", gii$data_info$Dimensionality));
      }

      annot_data = as.integer(gii$data[[element_index]]); # note that as.integer() turns the (1 column) matrix into a vector.
      if(data_only | is.na(key_column_name)) {
        return(annot_data);
      }

      # Note: gifti labels seem to be more like a mask or an annotation: they assign a value to each vertex of the surface instead of listing
      # all vertices which are part of the label. Reading them as a label in the FreeSurfer sense potentially means losing
      # information (if they contain more than 2 region types). If they only contain positive/negative labels, it is fine.
      num_regions_in_annot = nrow(gii$label);
      colortable_raw = as.data.frame(gii$label, stringsAsFactors = FALSE);
      colortable = matrix(rep(0.0, num_regions_in_annot * 5L), ncol = 5L);
      colnames(colortable) = c('r', 'g', 'b', 'a', 'code');

      missing_columns = c();
      colortable_column_index=1L;
      for(coln in rgb_column_names) {
        if(! is.na(coln)) {
          if(coln %in% colnames(colortable_raw)) {
            colortable[,colortable_column_index] = as.double(colortable_raw[[coln]]);
          } else {
            missing_columns = c(missing_columns, coln);
          }
        }
        colortable_column_index = colortable_column_index + 1L;
      }

      if(length(missing_columns) > 0L) {
        warning(sprintf("Gifti annotation/label is missing expected colortable columns: '%s' (available columns: '%s'). Fix parameter 'rgb_column_names'.\n", paste(missing_columns, collapse = ", "), paste(colnames(colortable_raw), collapse = ", ")));
      }

      # Get the struct_names manually, the gifti package does not provide them.
      xml = xml2::read_xml(filepath);
      label_nodes = xml2::xml_find_all(xml, './/Label');
      struct_names = xml2::xml_text(label_nodes);

      if(key_column_name %in% colnames(colortable_raw)) {
        colortable[,5] = as.integer(colortable_raw[[key_column_name]]);
      } else {
        print("Label table cdata (region names):");
        print(struct_names);
        print("Label table attribute columns:");
        print(colortable_raw);
        stop(sprintf("Specified key column '%s' does not exist in lable table attribute columns (available columns: '%s'). Fix parameter 'key_column_name'.\n", key_column_name, paste(colnames(colortable_raw), collapse = ", ")));
      }

      r = colortable[,1];
      g = colortable[,2];
      b = colortable[,3];
      a = colortable[,4];
      code = colortable[,5];
      if(max(r) > 1.1) {
        hex_color_string_rgb = grDevices::rgb(r/255., g/255., b/255.);
        hex_color_string_rgba = grDevices::rgb(r/255., g/255., b/255., a/255);
      } else {
        hex_color_string_rgb = grDevices::rgb(r, g, b);
        hex_color_string_rgba = grDevices::rgb(r, g, b, a);
      }



      colortable_df = data.frame(struct_names, r, g, b, a, code, hex_color_string_rgb, hex_color_string_rgba, stringsAsFactors = FALSE);
      colnames(colortable_df) = c("struct_name", "r", "g", "b", "a", "code", "hex_color_string_rgb", "hex_color_string_rgba");
      return(colortable_df);
    }

  } else {
    stop("The 'gifti' package must be installed to use this functionality.");
  }
}
