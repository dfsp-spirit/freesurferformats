# Functions for writing annotations and related data.


#' @title Write colortable file in FreeSurfer ASCII LUT format.
#'
#' @description Write the colortable of an annotation to a text file in FreeSurfer ASCII colortable lookup table (LUT) format. An example file is `FREESURFER_HOME/FreeSurferColorLUT.txt`.
#'
#' @param filepath, string. Full path to the output colormap file.
#'
#' @param annot An annotation, as returned by [read.fs.annot()]. If you want to assign specific indices, you can add a column named 'struct_index' to the data.frame \code{annot$colortable_df}. If there is no such columns, the indices will be created automatically in the order of the regions, starting at zero.
#'
#' @return the data.frame that was written to the LUT file.
#'
#' @family atlas functions
#' @family colorLUT functions
#'
#' @export
write.fs.colortable.from.annot <- function(filepath, annot) {

  colortable = annot$colortable;

  if(is.null(annot$colortable_df$struct_index)) {
    struct_index = seq(0, colortable$num_entries - 1);
  } else {
    struct_index = annot$colortable_df$struct_index;
  }

  colormap_output_df = data.frame("struct_index"=struct_index, "struct_name"=colortable$struct_names, "r"=colortable$table[,1], "g"=colortable$table[,2], "b"=colortable$table[,3], "a"=colortable$table[,4]);

  write.table(colormap_output_df, file = filepath, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  return(invisible(colormap_output_df));
}


#' Write annotation to binary file.
#'
#' @description Write an annotation to a FreeSurfer binary format annotation file in the new format (v2). An annotation (or brain parcellation) assigns each vertex to a label (or region). One of the regions is often called 'unknown' or similar and all vertices which are not relevant for the parcellation are assigned this label.
#'
#' @param filepath string, path to the output file
#'
#' @param num_vertices integer, the number of vertices of the surface
#'
#' @param colortable dataframe that contains one region per row. Required columns are: 'struct_name': character string, the region name. 'r': integer in range 0-255, the RGB color value for the red channel. 'g': same for the green channel. 'b': same for the blue channel. 'a': the alpha (transparency) channel value. Optional columns are: 'code': the color code. Will be computed if not set. Note that you can pass the dataframe returned by [read.fs.annot()] as 'colortable_df'.
#'
#' @param labels_as_colorcodes vector of *n* integers. The first way to specify the labels. Each integer is a colorcode, that has been computed from the RGBA color values of the regions in the colortable as \code{r + g*2^8 + b*2^16 + a*2^24}. If you do not already have these color codes, it is way easier to set this to NULL and define the labels as indices into the colortable, see parameter labels_as_indices_into_colortable.
#'
#' @param labels_as_indices_into_colortable vector of *n* integers, the second way to specify the labels. Each integer is an index into the rows of the colortable. Indices start with 1. This parameter and labels_as_colorcodes are mutually exclusive, but required.
#'
#' @export
write.fs.annot <- function(filepath, num_vertices, colortable, labels_as_colorcodes=NULL, labels_as_indices_into_colortable=NULL) {

  if(! is.integer(num_vertices)) {
    stop("Parameter 'num_vertices' must be an integer.");
  }

  if((is.null(labels_as_colorcodes) && is.null(labels_as_indices_into_colortable)) || (! (is.null(labels_as_colorcodes) || is.null(labels_as_indices_into_colortable)))) {
    stop("Exactly one of the parameters 'labels_as_colorcodes' and 'labels_as_indices_into_colortable' must be NULL.");
  }

  for(req_column in c("struct_name", "r", "g", "b", "a")) {
    if(is.null(colortable[[req_column]])) {
      stop(sprintf("Parameter 'colortable' must have a column named '%s'.\n", req_column));
    }
  }

  if(is.null(labels_as_colorcodes)) {
    if(length(labels_as_indices_into_colortable) != num_vertices) {
      stop(sprintf("Number of vertices (%d) must match length of parameter 'labels_as_indices_into_colortable' (%d).\n", num_vertices, length(labels_as_indices_into_colortable)));
    }

    if(is.null(colortable$code)) { # Compute the label codes for the regions of the colortable.
      colortable$code = colortable$r + colortable$g*2^8 + colortable$b*2^16 + colortable$a*2^24;
    }

    # Now compute the label code for each vertex from the label_indices and the colortable label_codes
    labels = colortable$code[labels_as_indices_into_colortable];
  } else {
    labels = labels_as_colorcodes;
  }

  if(num_vertices != length(labels)) {
    stop(sprintf("The number of vertices (%d) does not match the number of labels (%d). Each vertex must be assigned to a label.\n", num_vertices, length(labels)));
  }

  if(guess.filename.is.gzipped(filepath, gz_entensions=c(".gz"))) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  vertices = seq(0, num_vertices -1);
  verts_and_labels = c(rbind(vertices, labels));

  writeBin(as.integer(num_vertices * 2), fh, endian = "big");
  writeBin(verts_and_labels, fh, size = 4, endian = "big");
  cat(sprintf("Writing %d vertices and labels...\n", length(verts_and_labels)));

  if(! is.null(colortable)) {
    num_regions = nrow(colortable);
    ctable_format_version = -2L;    # If this field contains a positive number, the ctable is in 'old' format
                                    # and this is interpreted as the number of regions. If it is a negative number,
                                    # the number indicates the file format version number (new format).

    writeBin(as.integer(1L), fh, size = 4, endian = "big");  # flag 'has_colortable' = yes
    writeBin(ctable_format_version, fh, size = 4, endian = "big");
    writeBin(num_regions, fh, size = 4, endian = "big");
    writeChar("/tmp/fsbrain/some.lut", fh);    # The file path to the LUT file this annt is using. Does not apply to this function, so write whatever.
    writeBin(num_regions, fh, size = 4, endian = "big");   # Yes, this is duplicated.

    cat(sprintf("#######Handling %d regions...\n", num_regions));
    for (region_idx in seq_len(num_regions)) {
      writeBin(as.integer(region_idx - 1L), fh, size = 4, endian = "big");
      region_name = as.character(colortable$struct_name[[region_idx]]);
      #cat(sprintf("### region %d name is '%s'\n", region_idx, region_name));
      writeBin(as.integer(nchar(region_name)), fh, size = 4, endian = "big");
      writeChar(region_name, fh);

      # write colors
      writeBin(as.integer(colortable$r[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$g[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$b[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$a[region_idx]), fh, size = 4, endian = "big");
      #cat(sprintf("### region %d done\n", region_idx));
    }

  } else {
    stop("Parameter 'colortable' must not be NULL.");
  }

  close(fh);
}


