
#' @title Add metadata to GIFTI XML tree.
#'
#' @param xmltree XML tree from xml2
#'
#' @param metadata_named_list named list, the metadata entries
#'
#' @param as_cdata logical, whether to wrap the value in cdata tags
#'
#' @return the modified tree.
#'
#' @note Assumes that there already exists a global MetaData node. Also not that this is not supposed to be used for adding metadata to datarrays.
#'
#' @examples
#'   xmltree = gifti_xml(list(rep(3.1, 3L), matrix(seq(6)+0.1, nrow=2L)));
#'   newtree = gifti_xml_add_global_metadata(xmltree, list("User"="Me", "Weather"="Great"));
#'   gifti_xsd = "https://www.nitrc.org/frs/download.php/158/gifti.xsd";
#'   xml2::xml_validate(newtree, xml2::read_xml(gifti_xsd));
#'
#' @export
gifti_xml_add_global_metadata <- function(xmltree, metadata_named_list, as_cdata=TRUE) {
  xpath='.//MetaData';
  for(name in names(metadata_named_list)) {
    value = metadata_named_list[[name]];
    md_node = xml_node_gifti_MD(name, value, as_cdata=as_cdata);
    metadata_node = xml2::xml_find_first(xmltree, xpath);
    xml2::xml_add_child(metadata_node, md_node);
  }
  return(xmltree);
}


#' @title Create XML GIFTI metadata node.
#'
#' @param name character string, the metadata name
#'
#' @param value character string, the metadata value
#'
#' @param as_cdata logical, whether to wrap the value in cdata tags
#'
#' @note This creates an MD note, not the outer MetaData node.
#'
#' @return XML tree from xml2
#'
#' @keywords internal
xml_node_gifti_MD <- function(name, value, as_cdata=TRUE) {
  if(as_cdata) {
    name = cdata(name);
    value = cdata(value);
  }
  return(xml2::read_xml(paste('<MD><Name>', name, '</Name><Value>', value, '</Value></MD>', sep="")));
}


#' @title Create CDATA element string from string.
#'
#' @param string character string, the input string, freeform text. Must not contain the cdata start and end tags.
#'
#' @return character string, the input wrapped in the cdata tags
#'
#' @note This returns a string, not an XML node. See \code{\link[xml2]{xml_cdata}} if you want a node.
#'
#' @export
cdata <- function(string) {
  cdata_start_tag = "<![CDATA[";
  cdata_end_tag = "]]>";
  if(grepl(string, cdata_start_tag, fixed = TRUE)) {
    stop(sprintf("Input string must not contain the cdata start tag '%s'.\n", cdata_start_tag));
  }
  if(grepl(string, cdata_end_tag, fixed = TRUE)) {
    stop(sprintf("Input string must not contain the cdata end tag '%s'.\n", cdata_end_tag));
  }
  return(paste(cdata_start_tag, string, cdata_end_tag, sep=""));
}


#' @title Create XML GIFTI Label node.
#'
#' @param attributes named list, the attributes
#'
#' @param value the text contents of the node, usually the region name
#'
#' @param as_cdata logical, whether to wrap the value in cdata tags
#'
#' @return XML node from xml2
#'
#' @importFrom xml2 read_xml xml_set_attrs
#' @keywords internal
xml_node_gifti_label <- function(value, attributes=list(), as_cdata=TRUE) {
  if(! is.list(attributes)) {
    stop("Parameter 'value' must be a named list.");
  }
  if(as_cdata) {
    value = cdata(value);
  }
  label_node = xml2::read_xml(paste('<Label>', value, '</Label>', sep=''));
  xml2::xml_set_attrs(label_node, attributes);
  return(label_node);
}


#' @title Create XML GIFTI LabelTable node.
#'
#' @return XML node from xml2
#'
#' @importFrom xml2 read_xml
#' @keywords internal
xml_node_gifti_label_table <- function(attributes=list()) {
  return(xml2::read_xml('<LabelTable></LabelTable>'));
}


#' @title Add a standard label tabel for to a GIFTI XML tree.
#'
#' @description This label table is suitable for labels in the FreeSurfer sense, i.e., if only a positive label (1) and a negative label (0) exist in the label data.
#'
#' @param xmltree an XML tree from xml2, typically the return value from \code{\link[freesurferformats]{gifti_xml}}.
#'
#' @return XML tree from xml2, the modified tree with the LabelTable added below the root node.
#'
#' @importFrom xml2 xml_add_child
#' @keywords internal
giftixml_add_labeltable_posneg <- function(xmltree) {
  label_table_node = xml_node_gifti_label_table();
  xml2::xml_add_child(label_table_node, xml_node_gifti_label('negative', attributes=list('Key'=0L)));
  xml2::xml_add_child(label_table_node, xml_node_gifti_label('positive', attributes=list('Key'=1L)));
  xml2::xml_add_child(xmltree, label_table_node);
  return(xmltree);
}


#' @title Add a label tabel from an annotation to a GIFTI XML tree.
#'
#' @description Computes the LabelTable XML node for the given annotation and adds it to the XML tree.
#'
#' @param xmltree an XML tree from xml2, typically the return value from \code{\link[freesurferformats]{gifti_xml}}.
#'
#' @param annot an fs.annotation, the included data will be used to compute the LabelTable node
#'
#' @return XML tree from xml2, the modified tree with the LabelTable added below the root node.
#'
#' @importFrom xml2 xml_add_child
#' @keywords internal
giftixml_add_labeltable_from_annot <- function(xmltree, annot) {
  label_table_node = xml_node_gifti_label_table_from_annot(annot);
  xml2::xml_add_child(xmltree, label_table_node);
  return(xmltree);
}


#' @title Compute LabelTable node from annotation.
#'
#' @param annot an fs.annotation, the included data will be used to compute the LabelTable node
#'
#' @return XML tree from xml2, the LabelTable and its child nodes
#'
#' @importFrom xml2 xml_add_child
#' @keywords internal
xml_node_gifti_label_table_from_annot <- function(annot) {
  label_table_node = xml_node_gifti_label_table();
  if(! is.null(annot$colortable_df)) {
    for(row_idx in seq.int(nrow(annot$colortable_df))) {
      sr = annot$colortable_df[row_idx, ];
      xml2::xml_add_child(label_table_node, xml_node_gifti_label(sr$struct_name, attributes=list('Red'=sr$r, 'Green'=sr$g, 'Blue'=sr$b, 'Alpha'=sr$a, 'Key'=sr$code)));
    }
  }
  return(label_table_node);
}


