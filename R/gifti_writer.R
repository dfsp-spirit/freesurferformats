# This file contains basic functions for generating valid GIFTI XML documents and writing them to files.
#
# Note that GIFTI is a very versatile and complex format, and one can write a lot of different things in different formats.
# While this is great, it also means that it is not guaranteed that a software x will be able to interprete the data stored in a GIFTI file by software y in the
# way intended by y (even though the file is technically a valid GIFTI file).
# The functions in here leave the user the ability to edit the generated XML trees (e.g., with the xml2 package), so you should be able to use these functions
# to write data exporters for any software.
#
# Some writers for how a specific software stores data of certain typpes (surface, measurements/morphometry, labels) in GIFTI format files are implemented in separate
# source files. E.g., writers for FreeSurfer can be found in gifti_writer_freesurfer.R.

#' @title Get GIFTI XML representation of data.
#'
#' @description Creates a GIFTI XML tree from your datasets (vectors and matrices). The tree can be further modified to add additional data, or written to a file as is to produce a valid GIFTI file (see \code{\link{gifti_xml_write}}).
#'
#' @param data_array list of data vectors and/or data matrices.
#'
#' @param intent vector of NIFTI intent strings for the data vectors in 'data_array' parameter, see \code{\link[gifti]{convert_intent}}. Example: 'NIFTI_INTENT_SHAPE'. See \url{https://nifti.nimh.nih.gov/nifti-1/documentation/nifti1fields/nifti1fields_pages/group__NIFTI1__INTENT__CODES.html}.
#'
#' @param datatype vector of NIFTI datatype strings. Example: 'NIFTI_TYPE_FLOAT32'. Should be suitable for your data.
#'
#' @param encoding vector of encoding definition strings. One of 'ASCII', 'Base64Binary', 'GZipBase64Binary'.
#'
#' @param endian vector of endian definition strings. One of 'LittleEndian' or 'BigEndian'. See \code{\link[gifti]{convert_endian}}.
#'
#' @param force logical, whether to force writing the data, even if issues like a mismatch of datatype and data values are detected.
#'
#' @return xml tree, see xml2 package. One could modify this tree as needed using xml2 functions, e.g., add metadata.
#'
#' @references \url{https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf}
#'
#' @note Unless you want to modify the returned tree manually, you should not need to call this function. Use \code{\link{gifti_writer}} instead.
#'
#' @seealso The example for \code{\link{gifti_xml_write}} shows how to modify the tree.
#'
#' #' @examples
#'   my_data_sets = list(rep(3.1, 3L), matrix(seq(6)+0.1, nrow=2L));
#'   xmtree = gifti_xml(my_data_sets, datatype='NIFTI_TYPE_FLOAT32');
#'   # Verify that the tree is a valid GIFTI file:
#'   xml2::xml_validate(xmltree, xml2::read_xml("https://www.nitrc.org/frs/download.php/158/gifti.xsd"));
#'
#' @importFrom xml2 xml_new_root xml_set_attr xml_add_child read_xml
#' @export
gifti_xml <- function(data_array, intent='NIFTI_INTENT_SHAPE', datatype='NIFTI_TYPE_FLOAT32', encoding='GZipBase64Binary', endian='LittleEndian', force=FALSE) {
  if( ! is.list(data_array)) {
    stop("Parameter 'data_array' must be a list.");
  }

  supported_encodings = c('ASCII', 'Base64Binary', 'GZipBase64Binary');

  num_data_arrays = length(data_array);
  dataarray_contains_matrices = any(lapply((lapply(data_array, dim)), length) > 0L);

  if(num_data_arrays > 1L) {
    if(length(intent) == 1L) {
      intent = rep(intent, num_data_arrays);
    }
    if(length(datatype) == 1L) {
      datatype = rep(datatype, num_data_arrays);
    }
    if(length(encoding) == 1L) {
      encoding = rep(encoding, num_data_arrays);
    }
    if(length(endian) == 1L) {
      endian = rep(endian, num_data_arrays);
    }
  }

  dim0 = rep(1L, num_data_arrays);                               # gets filled later
  dim1 = rep(1L, num_data_arrays);                               # gets filled later
  dimensionality = rep(1L, num_data_arrays);                     # gets filled later
  array_indexing_order = rep("RowMajorOrder", num_data_arrays);  # currently fixed

  root = xml2::xml_new_root("GIFTI", 'Version' = "1.0", 'NumberOfDataArrays'=num_data_arrays);
  metadata = xml2::xml_add_child(root, read_xml("<MetaData></MetaData>"));
  xml2::xml_add_child(metadata, read_xml("<MD><Name>Date</Name><Value>2020-01-01</Value></MD>"));


  da_index = 1L;
  data_is_matrix = FALSE;
  for(da in data_array) {
    if(is.vector(da)) {
      dim0[da_index] = length(da);
      dim1[da_index] = 1L;
      dimensionality[da_index] = 1L;
    } else if(is.matrix(da)) {
      data_is_matrix = TRUE;
      dimensionality[da_index] = 2L;
      dim0[da_index] = dim(da)[1];
      dim1[da_index] = dim(da)[2];
      if(array_indexing_order[da_index] == "RowMajorOrder") {
        da = as.vector(t(da));
      } else if(array_indexing_order[da_index] == "ColumnMajorOrder") {
        da = as.vector((da));
      } else {
        stop(sprintf("Dataarray # %d: invalid array_indexing_order, must be 'RowMajorOrder' or 'ColumnMajorOrder'.\n", da_index));
      }
    } else {
      stop("The data_arrays must be of type vector or matrix.");
    }

    if(! encoding[da_index] %in% supported_encodings) {
      stop(sprintf("Dataarray # %d: invalid encoding '%s'.\n", da_index, encoding[da_index]));
    }
    if(! endian[da_index] %in% c('LittleEndian', 'BigEndian')) {
      stop(sprintf("Dataarray # %d: invalid endian '%s'.\n", da_index, endian[da_index]));
    }

    check_data_and_settings_consistency(da_index, da, datatype[da_index], intent[da_index], force=force);

    data_array_node = xml2::read_xml("<DataArray/>");
    xml2::xml_set_attr(data_array_node, 'Dimensionality', dimensionality[da_index]);
    xml2::xml_set_attr(data_array_node, 'Dim0', dim0[da_index]);

    if(dataarray_contains_matrices) {   # We only need this attribute if any dataarray has more than 1 dimension. But if so, we need it for all datasets.
      xml2::xml_set_attr(data_array_node, 'Dim1', dim1[da_index]);
    }

    xml2::xml_set_attr(data_array_node, 'Encoding', encoding[da_index]);
    xml2::xml_set_attr(data_array_node, 'DataType', datatype[da_index]);
    xml2::xml_set_attr(data_array_node, 'Intent', intent[da_index]);
    xml2::xml_set_attr(data_array_node, 'Endian', endian[da_index]);

    xml2::xml_set_attr(data_array_node, 'ExternalFileName', '');                   # not supported atm
    xml2::xml_set_attr(data_array_node, 'ExternalFileOffset', '');                 # not supported atm
    xml2::xml_set_attr(data_array_node, 'ArrayIndexingOrder', array_indexing_order[da_index]);

    data_array_node_added = xml2::xml_add_child(root, data_array_node);
    data_array_metadata = xml2::xml_add_child(data_array_node_added, xml2::read_xml("<MetaData></MetaData>"));
    encoded_data = gifti::data_encoder(da, encoding = encoding[da_index], datatype = datatype[da_index], endian = endian[da_index]);
    data_node = xml2::read_xml(sprintf("<Data>%s</Data>", encoded_data));
    xml2::xml_add_child(data_array_node_added, data_node);
    da_index = da_index + 1L;
  }
  return(root);
}


#' @title Warn about common errors in combining data and datatype.
#'
#' @param index positive integer, the dataarray index to report. Makes it easier for the user to find the broken one.
#'
#' @param data vector or matrix, the data to write to the GIFTI file. Checked against the datatype.
#'
#' @param datatype NIFTI datatype string, the datatype to use when writing to the GIFTI file. Checked against the data.
#'
#' @param intent NIFTI intent string, checked independently. In no way do we check whether it makes sense for the data.
#'
#' @note The checks in here are in no way exhaustive.
#'
#' @keywords internal
#' @importFrom gifti convert_intent
check_data_and_settings_consistency <- function(index, data, datatype, intent, force=FALSE) {
  msg = NULL;
  if(is.integer(data) & startsWith(datatype, "NIFTI_TYPE_FLOAT")) {
    msg = sprintf("Dataset # %d in file will be corrupted: integer R data passed, and written as '%s'.\n", index, datatype);
  }

  if(gifti::convert_intent(intent) == "unknown") {
    msg = sprintf("Dataset # %d: Invalid NIFTI intent '%s'.\n", index, intent);
  }

  # Should check more here.

  if(! is.null(msg)) {
    if(force) {
      warning(msg);
    } else {
      stop(msg);
    }
  }
}


#' @title Write XML tree to a gifti file.
#'
#' @param filepath path to the output gifti file
#'
#' @param xmltree
#'
#' @param options output options passed to \code{\link[xml2]{write_xml}}.
#'
#' @references \url{https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf}
#'
#' @examples
#'   outfile = tempfile(fileext = 'gii');
#'   my_data_sets = list(rep(3.1, 3L), matrix(seq(6)+0.1, nrow=2L));
#'   xmltree = gifti_xml(my_data_sets, datatype='NIFTI_TYPE_FLOAT32');
#'   # Here we add global metadata:
#'   xmltree = gifti_xml_add_global_metadata(xmltree, list("User"="Me", "Day"="Monday"));
#'   # Validating your XML never hurts
#'   xml2::xml_validate(xmltree, xml2::read_xml("https://www.nitrc.org/frs/download.php/158/gifti.xsd"));
#'   gifti_xml_write(outfile, xmltree);  # Write your custom tree to a file.
#'
#' @export
gifti_xml_write <- function(filepath, xmltree, options=c('as_xml', 'format')) {
  return(invisible(xml2::write_xml(xmltree, file=filepath, options=options)));
}


#' @title Write data to a gifti file.
#'
#' @param filepath path to the output gifti file
#'
#' @param ... parameters passed to \code{\link{gifti_xml}}.
#'
#' @references \url{https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf}
#'
#' @examples
#'   outfile = tempfile(fileext = 'gii');
#'   gifti_writer(outfile, list(rep(3.1, 3L), matrix(seq(6), nrow=2L)), datatype=c('NIFTI_TYPE_FLOAT32', 'NIFTI_TYPE_INT32');
#'
#' @export
gifti_writer <- function(filepath, ...) {
  if(! is.character(filepath)) {
    stop("Parameter 'filepath' must be a character string.");
  }
  xmltree = gifti_xml(...);
  return(invisible(gifti_xml_write(filepath, xmltree)));
}
