#' @title Read file in FreeSurfer annotation format
#'
#' @description Read a data annotation file in FreeSurfer format. Such a file assigns a label and a color to each vertex of a brain surface. The assignment of labels to vertices is based on at atlas or brain parcellation file. Typically the atlas is available for some standard template subject, and the labels are assigned to another subject by registering it to the template.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/label/lh.aparc.annot', which contains the annotation based on the Desikan-Killiany Atlas for the left hemisphere of bert.
#'
#' @param filepath, string. Full path to the input curv file.
#'
#' @return ???
#'
#'
#' @export
read.fs.annot <- function(filepath) {

    fh = file(filepath, "rb");

    num_verts_and_labels = readBin(fh, integer(), n = 1, endian = "big");
    verts_and_labels = readBin(fh, integer(), n = num_verts_and_labels, endian = "big");

    verts = verts_and_labels[seq(1L, length(verts_and_labels), 2L)]
    labels = verts_and_labels[seq(2L, length(verts_and_labels), 2L)]

    has_colortable = readBin(fh, integer(), n = 1, endian = "big");

    if (has_colortable) {
        ctable_num_entries = readBin(fh, integer(), n = 1, endian = "big");

        if(ctable_num_entries > 0) {
          # num_colortable_entries is really the number of entries
          colortable <- list("num_entries" = num_colortable_entries)
          ctable_length = readBin(fh, integer(), n = 1, endian = "big");

          ctab_orig = readBin(fh, char(), n = ctable_length, endian = "big");
          ctab_orig = ctab_orig[1:length(ctab_orig)-1]  # remove last element

        } else {
          # num_colortable_entries is a version code (actually, the abs value is).
          version = -num_colortable_entries;
          if(version == 2) {

          }
          else {
              error("Unsupported annotation file version.");
          }

        }

    }

    return(data);
}
