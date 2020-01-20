# Functions for writing annotations and related data.


#' @title Write colortable file in FreeSurfer ASCII LUT format.
#'
#' @description Write the colortable to a text file in FreeSurfer ASCII colortable lookup table (LUT) format. An example file is `FREESURFER_HOME/FreeSurferColorLUT.txt`.
#'
#' @param filepath, string. Full path to the output colormap file.
#'
#' @param colortable data.frame, a colortable as read by \code{\link[freesurferformats]{read.fs.colortable}}. Must contain the following columns: 'struct_name': character string, the label name. 'r': integer in range 0-255, the RGBA color value for the red channel. 'g': same for green channel. 'b': same for blue channel. 'a': same for alpha (transparency) channel. Can contain the following column: 'struct_index': integer, index of the struct entry. If this column does not exist, sequential indices starting at zero are created.
#'
#' @return the written dataframe, invisible. Note that this is will contain a column named 'struct_index', no matter whether the input colortable contained it or not.
#'
#' @family atlas functions
#' @family colorLUT functions
#'
#' @export
write.fs.colortable <- function(filepath, colortable) {

  if(! is.data.frame(colortable)) {
    stop("Parameter 'colortable' must be a dataframe.");
  }

  for(req_column in c("struct_name", "r", "g", "b", "a")) {
    if(is.null(colortable[[req_column]])) {
      stop(sprintf("Parameter 'colortable' must have a column named '%s'.\n", req_column));
    }
  }

  if(is.null(colortable$struct_index)) {
    colortable$struct_index = seq(0, nrow(colortable) - 1);
  }

  write.table(colortable, file = filepath, quote = FALSE, sep = " ", row.names = FALSE, col.names = FALSE);
  return(invisible(colortable));
}


#' Write annotation to binary file.
#'
#' @description Write an annotation to a FreeSurfer binary format annotation file in the new format (v2). An annotation (or brain parcellation) assigns each vertex to a label (or region). One of the regions is often called 'unknown' or similar and all vertices which are not relevant for the parcellation are assigned this label.
#'
#' @param filepath string, path to the output file
#'
#' @param num_vertices integer, the number of vertices of the surface. Must be given unless parameter `fs.annot` is not NULL.
#'
#' @param colortable dataframe that contains one region per row. Required columns are: 'struct_name': character string, the region name. 'r': integer in range 0-255, the RGB color value for the red channel. 'g': same for the green channel. 'b': same for the blue channel. 'a': the alpha (transparency) channel value. Optional columns are: 'code': the color code. Will be computed if not set. Note that you can pass the dataframe returned by \code{\link[freesurferformats]{read.fs.annot}} as 'colortable_df'. Only required if `labels_as_indices_into_colortable` is used.
#'
#' @param labels_as_colorcodes vector of *n* integers. The first way to specify the labels. Each integer is a colorcode, that has been computed from the RGBA color values of the regions in the colortable as \code{r + g*2^8 + b*2^16 + a*2^24}. If you do not already have these color codes, it is way easier to set this to NULL and define the labels as indices into the colortable, see parameter `labels_as_indices_into_colortable`.
#'
#' @param labels_as_indices_into_colortable vector of *n* integers, the second way to specify the labels. Each integer is an index into the rows of the colortable. Indices start with 1. This parameter and `labels_as_colorcodes` are mutually exclusive, but required.
#'
#' @param fs.annot instance of class `fs.annot`. If passed, this takes precedence over all other parameters and they should all be NULL (with the exception of `filepath`).
#'
#' @examples
#' \donttest{
#'    # Load annotation
#'    annot_file = system.file("extdata", "lh.aparc.annot.gz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'    annot = read.fs.annot(annot_file);
#'
#'    # New method: write the annotation instance:
#'    write.fs.annot(tempfile(fileext=".annot"), fs.annot=annot);
#'
#'    # Old method: write it from its parts:
#'    write.fs.annot(tempfile(fileext=".annot"), length(annot$vertices),
#'     annot$colortable_df, labels_as_colorcodes=annot$label_codes);
#' }
#'
#' @family atlas functions
#' @export
write.fs.annot <- function(filepath, num_vertices=NULL, colortable=NULL, labels_as_colorcodes=NULL, labels_as_indices_into_colortable=NULL, fs.annot=NULL) {

  if(is.fs.annot(fs.annot)) {
    num_vertices = length(fs.annot$label_codes);
    colortable = fs.annot$colortable_df;
    labels = fs.annot$label_codes;
  } else {

    if(! is.integer(num_vertices)) {
      stop("Parameter 'num_vertices' must be an integer.");
    }

    if((is.null(labels_as_colorcodes) && is.null(labels_as_indices_into_colortable)) || (! (is.null(labels_as_colorcodes) || is.null(labels_as_indices_into_colortable)))) {
      stop("Exactly one of the parameters 'labels_as_colorcodes' and 'labels_as_indices_into_colortable' must be NULL.");
    }


    if(is.null(labels_as_colorcodes)) {
      if(length(labels_as_indices_into_colortable) != num_vertices) {
        stop(sprintf("Number of vertices (%d) must match length of parameter 'labels_as_indices_into_colortable' (%d).\n", num_vertices, length(labels_as_indices_into_colortable)));
      }

      if(is.null(colortable)) {
        stop("Parameter 'colortable' must not be NULL unless labels are passed as colorcodes via parameter 'labels_as_colorcodes'.");
      }

      for(req_column in c("struct_name", "r", "g", "b", "a")) {
        if(is.null(colortable[[req_column]])) {
          stop(sprintf("Parameter 'colortable' must have a column named '%s'.\n", req_column));
        }
      }

      if(is.null(colortable$code)) { # Compute the label codes for the regions of the colortable.
        colortable$code = colortable$r + colortable$g*2^8 + colortable$b*2^16 + colortable$a*2^24;
      }

      # Now compute the label code for each vertex from the label_indices and the colortable label_codes
      labels = colortable$code[labels_as_indices_into_colortable];
    } else {
      labels = labels_as_colorcodes;
    }
  }

  if(num_vertices != length(labels)) {
    stop(sprintf("The number of vertices (%d) does not match the number of labels (%d). Each vertex must be assigned to a label.\n", num_vertices, length(labels)));
  }

  if(guess.filename.is.gzipped(filepath, gz_extensions=c(".gz"))) {
    fh = gzfile(filepath, "wb");
  } else {
    fh = file(filepath, "wb", blocking = TRUE);
  }

  vertices = seq(0, num_vertices -1);
  verts_and_labels = as.integer(c(rbind(vertices, labels)));  # indices and label codes are writting in alternating style (vert0, label1, ver1, label1, vert2, ...)

  if(length(verts_and_labels) != (num_vertices * 2)) {
    stop(sprintf("Incorrect length of verts_and_labels: expected %d, found %d.\n", (num_vertices * 2), length(verts_and_labels)));
  }

  writeBin(as.integer(num_vertices), fh, endian = "big");   # write the number of label values that follow. Note that this is 1/2 of the actual data values!
  writeBin(verts_and_labels, fh, size = 4, endian = "big");     # write the actual data values

  if(! is.null(colortable)) {
    num_regions = nrow(colortable);
    ctable_format_version = -2L;    # If this field contains a positive number, the ctable is in 'old' format
                                    # and this is interpreted as the number of regions. If it is a negative number,
                                    # the number indicates the file format version number (new format).

    writeBin(as.integer(1L), fh, size = 4, endian = "big");  # flag 'has_colortable' = yes
    writeBin(ctable_format_version, fh, size = 4, endian = "big");     # write version number
    writeBin(num_regions, fh, size = 4, endian = "big");               # write num entries in ctable

    dev_ct_filename = "/tmp/fsbrain/some.lut";
    writeBin(nchar(dev_ct_filename), fh, size = 4, endian = "big");
    writeChar(dev_ct_filename, fh, eos=NULL);    # The file path to the LUT file this annt is using. Does not apply to this function, so write whatever.

    writeBin(num_regions, fh, size = 4, endian = "big");   # Yes, this is duplicated.

    for (region_idx in seq_len(num_regions)) {
      writeBin(as.integer(region_idx - 1L), fh, size = 4, endian = "big");
      region_name = as.character(colortable$struct_name[[region_idx]]);
      writeBin(nchar(region_name), fh, size = 4, endian = "big");
      writeChar(region_name, fh, eos=NULL);

      # write colors
      writeBin(as.integer(colortable$r[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$g[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$b[region_idx]), fh, size = 4, endian = "big");
      writeBin(as.integer(colortable$a[region_idx]), fh, size = 4, endian = "big");
    }

  } else {
    writeBin(as.integer(0L), fh, size = 4, endian = "big");  # flag 'has_colortable' = no
  }

  close(fh);
}


