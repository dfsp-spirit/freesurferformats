
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

