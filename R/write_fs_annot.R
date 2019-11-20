# Functions for writing annotations and related data.


#' @title Write colormap file in FreeSurfer ACII LUT format.
#'
#' @description Write the colormap of an annotation to a text file in FreeSurfer ASCII colormap lookup table (LUT) format.
#'
#' @param filepath, string. Full path to the output colormap file.
#'
#' @param annot An annotation, as returned by [read.fs.annot].
#'
#' @return the data.frame that was written to the LUT file.
#'
#' @family atlas functions
#'
#' @export
write.fs.colormap <- function(filepath, annot) {

  colortable = annot$colortable;

  region_index = seq(0, colortable$num_entries - 1);

  colormap_output_df = data.frame("region_index"=region_index, "region_name"=colortable$struct_names, "r"=colortable$table[,1], "g"=colortable$table[,2], "b"=colortable$table[,3], "a"=colortable$table[,4]);

  write.table(colormap_output_df, file = filepath, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  return(invisible(colormap_output_df));
}
