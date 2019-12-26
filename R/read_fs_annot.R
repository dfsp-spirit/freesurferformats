#' @title Read file in FreeSurfer annotation format
#'
#' @description Read a data annotation file in FreeSurfer format. Such a file assigns a label and a color to each vertex of a brain surface. The assignment of labels to vertices is based on at atlas or brain parcellation file. Typically the atlas is available for some standard template subject, and the labels are assigned to another subject by registering it to the template.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/label/lh.aparc.annot', which contains the annotation based on the Desikan-Killiany Atlas for the left hemisphere of bert.
#'
#' @param filepath string. Full path to the input annotation file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @param empty_label_name string. The region name to assign to regions with empty name. Defaults to 'unknown'. Set to NULL if you want to keep the empty region name.
#'
#' @return named list, enties are: "vertices" vector of n vertex indices, starting with 0. "label_codes": vector of n integers, each entry is a color code, i.e., a value from the 5th column in the table structure included in the "colortable" entry (see below). "label_names": the n brain structure names for the vertices, already retrieved from the colortable using the code. "hex_colors_rgb": Vector of hex color for each vertex.
#'      The "colortable" is another named list with 3 entries: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code. "colortable_df": The same information as a dataframe. Contains the extra columns "hex_color_string_rgb" and "hex_color_string_rgba" that hold the color as an RGB(A) hex string, like "#rrggbbaa".
#'
#' @family atlas functions
#'
#' @examples
#'     annot_file = system.file("extdata", "lh.aparc.annot.gz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     annot = read.fs.annot(annot_file);
#'
#' @export
read.fs.annot <- function(filepath, empty_label_name="unknown") {

    if(guess.filename.is.gzipped(filepath)) {
        fh = gzfile(filepath, "rb");
    } else {
        fh = file(filepath, "rb");
    }

    num_verts_and_labels = readBin(fh, integer(), n = 1, endian = "big");
    verts_and_labels = readBin(fh, integer(), n = num_verts_and_labels*2, endian = "big");

    verts = verts_and_labels[seq(1L, length(verts_and_labels), 2L)];
    labels = verts_and_labels[seq(2L, length(verts_and_labels), 2L)];

    return_list = list("vertices" = verts, "label_codes" = labels);

    has_colortable = readBin(fh, integer(), n = 1, endian = "big");

    if(has_colortable == 1L) {
        ctable_num_entries = readBin(fh, integer(), n = 1, endian = "big");

        if(ctable_num_entries > 0) {
            colortable = readcolortable_oldformat(fh, ctable_num_entries);
        } else {
            # If ctable_num_entries is negative, it is a version code (actually, the abs value is the version).
            version = -ctable_num_entries;
            if(version == 2) {
                ctable_num_entries = readBin(fh, integer(), n = 1, endian = "big");
                colortable = readcolortable(fh, ctable_num_entries);
            }
            else {
                stop(sprintf("Unsupported annotation file version '%d'.\n", version));
            }
        }
        return_list$colortable = colortable;

        # Compute convenience information from the data, so the user does not have to do it. This includes the labels and color for each vertex.
        struct_names = colortable$struct_names;
        r = colortable$table[,1];
        g = colortable$table[,2];
        b = colortable$table[,3];
        a = colortable$table[,4];
        code = colortable$table[,5];
        hex_color_string_rgb = grDevices::rgb(r/255., g/255., b/255.);
        hex_color_string_rgba = grDevices::rgb(r/255., g/255., b/255., a/255);
        colortable_df = data.frame(struct_names, r, g, b, a, code, hex_color_string_rgb, hex_color_string_rgba, stringsAsFactors = FALSE);
        colnames(colortable_df) = c("struct_name", "r", "g", "b", "a", "code", "hex_color_string_rgb", "hex_color_string_rgba");
        return_list$colortable_df = colortable_df;

        label_names = rep("", length(labels))
        hex_colors_rgb = rep("#333333", length(labels))
        nempty = 1;  # There could be more than 1 empty region, and we cannot match all of them to the same name.
        for (i in 1:length(colortable$struct_names)) {
          label_code = code[i];
          label_name = colortable$struct_names[i];
          hex_color_string_rgb = grDevices::rgb(colortable$table[i,1]/255., colortable$table[i,2]/255., colortable$table[i,3]/255.);
          if(nchar(empty_label_name) > 0 && nchar(label_name) == 0) {
            cat(sprintf("Replacing empty label name with '%s'\n", empty_label_name));
            label_name = paste(empty_label_name, nempty, sep="");
            nempty = nempty + 1;
          }
          label_names[labels==label_code] = label_name;
          hex_colors_rgb[labels==label_code] = hex_color_string_rgb;
        }
        return_list$label_names = label_names;
        return_list$hex_colors_rgb = hex_colors_rgb;

    }
    close(fh);
    class(return_list) = "fs.annot";
    return(return_list);
}


#' @title Print description of a brain atlas or annotation.
#'
#' @param x brain surface annotation or atlas with class `fs.annot`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.annot <- function(x, ...) {
  if(is.null(x$colortable)) {
    print(sprintf("Brain surface annotation without colortable for %d vertices containing %d unique region codes.", length(x$vertices), length(unique(x$label_codes))));
  } else {
    cat(sprintf("Brain surface annotation assigning %d vertices to %d brain regions.\n", length(x$vertices), nrow(x$colortable_df)));
    for(region_idx in seq_len(nrow(x$colortable_df))) {
      cat(sprintf("region #%d '%s': size %d vertices\n", region_idx, as.character(x$colortable_df$struct_name[[region_idx]]), sum(x$label_codes == x$colortable_df$code[[region_idx]])));
    }
  }
}



#' @title Read binary colortable in old format.
#'
#' @description Read an oldformat colortable from a connection to a binary file.
#'
#' @param fh file handle
#'
#' @param ctable_num_entries number of entries to read
#'
#' @return named list, the color table. The named entries are: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code.
#'
#'
#' @keywords internal
readcolortable_oldformat <- function(fh, ctable_num_entries) {

  colortable <- list("num_entries" = ctable_num_entries);

  num_chars_dev_filepath = readBin(fh, integer(), n = 1, endian = "big");
  dev_filepath = readChar(fh, num_chars_dev_filepath);

  colortable$struct_names = rep("", ctable_num_entries);
  colortable$table = matrix(0, nrow = ctable_num_entries, ncol = 5);

  for (i in 1:ctable_num_entries) {
    num_chars_struct_name = readBin(fh, integer(), n = 1, endian = "big");
    struct_name = readChar(fh, num_chars_struct_name);
    colortable$struct_names[i] = struct_name;
    r = readBin(fh, integer(), n = 1, endian = "big");   # red channel of color
    g = readBin(fh, integer(), n = 1, endian = "big");   # green channel of color
    b = readBin(fh, integer(), n = 1, endian = "big");   # blue channel of color
    a = readBin(fh, integer(), n = 1, endian = "big");   # alpha channel of color
    unique_color_label = r + g*2^8 + b*2^16 + a*2^24;

    colortable$table[i,1] = r;
    colortable$table[i,2] = g;
    colortable$table[i,3] = b;
    colortable$table[i,4] = a;
    colortable$table[i,5] = unique_color_label;
  }
  return(colortable);
}


#' @title Read binary colortable in v2 format.
#'
#' @description Read a v2 format colortable from a connection to a binary file.
#'
#' @param fh file handle
#'
#' @param ctable_num_entries number of entries to read
#'
#' @return named list, the color table. The named entries are: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code.
#'
#'
#' @keywords internal
readcolortable <- function(fh, ctable_num_entries) {

    colortable <- list("num_entries" = ctable_num_entries);
    ctab_orig_dev_filename_length = readBin(fh, integer(), n = 1, endian = "big");

    # Orginial filename of the colortable file that was used to create the atlas colortable (on the dev machine).
    ctab_orig_dev_filename = readChar(fh, ctab_orig_dev_filename_length);

    colortable$struct_names = rep("", ctable_num_entries);
    colortable$table = matrix(0, nrow = ctable_num_entries, ncol = 5);

    # There is another field here which also encodes the number of entries.
    ctable_num_entries_2nd = readBin(fh, integer(), n = 1, endian = "big");
    if(ctable_num_entries != ctable_num_entries_2nd) {
      warning(sprintf("Meta data on number of color table mismatches: %d versus %d.\n", ctable_num_entries, ctable_num_entries_2nd));
    }

    for (i in seq_len(ctable_num_entries)) {
        struct_idx = readBin(fh, integer(), n = 1, endian = "big") + 1L;

        # Index must not be negative:
        if (struct_idx < 0L) {
            stop(sprintf("Invalid struct index in color table entry #%d: index must not be negative but is '%d'.\n", i, struct_idx));
        }

        name_so_far = colortable$struct_names[struct_idx];
        # The same structure must not occur more than once (so the name should still be the empty string from the initialization when setting it):
        if (!identical(name_so_far, "")) {
            warning(sprintf("Annotation file entry #%d struct index %d: entry with identical name '%s' already hit, this must not happen. Brain structure names must be unique.\n", i, struct_idx, name_so_far));
        }
        entry_num_chars = readBin(fh, integer(), n = 1, endian = "big");

        brain_structure_name = readChar(fh, entry_num_chars);
        colortable$struct_names[i] = brain_structure_name;

        r = readBin(fh, integer(), n = 1, endian = "big");   # red channel of color
        g = readBin(fh, integer(), n = 1, endian = "big");   # green channel of color
        b = readBin(fh, integer(), n = 1, endian = "big");   # blue channel of color
        a = readBin(fh, integer(), n = 1, endian = "big");   # alpha channel of color
        unique_color_label = r + g*2^8 + b*2^16 + a*2^24;

        colortable$table[i,1] = r;
        colortable$table[i,2] = g;
        colortable$table[i,3] = b;
        colortable$table[i,4] = a;
        colortable$table[i,5] = unique_color_label;
    }

    return(colortable);
}


#' @title Read colortable file in FreeSurfer ASCII LUT format.
#'
#' @description Read a colortable from a text file in FreeSurfer ASCII colortable lookup table (LUT) format. An example file is `FREESURFER_HOME/FreeSurferColorLUT.txt`.
#'
#' @param filepath, string. Full path to the output colormap file.
#'
#' @param compute_colorcode logical, indicates whether the unique color codes should be computed and added to the returned data.frame as an extra integer column named 'code'. Defaults to FALSE.
#'
#' @return the data.frame that was read from the LUT file. It contains the following columns that were read from the file: 'struct_index': integer, index of the struct entry. 'struct_name': character string, the label name. 'r': integer in range 0-255, the RGBA color value for the red channel. 'g': same for green channel. 'b': same for blue channel. 'a': same for alpha (transparency) channel. If 'compute_colorcode' is TRUE, it also contains the following columns which were computed from the color values: 'code': integer, unique color identifier computed from the RGBA values.
#'
#' @examples
#'    lutfile = system.file("extdata", "colorlut.txt", package = "freesurferformats", mustWork = TRUE);
#'    colortable = read.fs.colortable(lutfile, compute_colorcode=TRUE);
#'    head(colortable);
#'
#' @family atlas functions
#' @family colorLUT functions
#'
#' @importFrom utils read.table
#' @export
read.fs.colortable <- function(filepath, compute_colorcode=FALSE) {

  colortable = read.table(filepath, header=FALSE, col.names=c("struct_index", "struct_name", "r", "g", "b", "a"), stringsAsFactors = FALSE);
  if(compute_colorcode) {
    colortable$code = colortable$r + colortable$g*2^8 + colortable$b*2^16 + colortable$a*2^24;
  }

  return(colortable);
}


#' @title Extract color lookup table (LUT) from annotation.
#'
#' @description Extract a colortable lookup table (LUT) from an annotation. Such a LUT can also be read from files like `FREESURFER_HOME/FreeSurferColorLUT.txt` or saved as a file, check the 'See Also' section below.
#'
#' @param annot An annotation, as returned by \code{\link[freesurferformats]{read.fs.annot}}. If you want to assign specific indices, you can add a column named 'struct_index' to the data.frame \code{annot$colortable_df}. If there is no such columns, the indices will be created automatically in the order of the regions, starting at zero.
#'
#' @param compute_colorcode logical, indicates whether the unique color codes should be computed and added to the returned data.frame as an extra integer column named 'code'. Defaults to FALSE.
#'
#' @return the colortable data.frame extracted from the annotation.
#'
#' @examples
#'     annotfile = system.file("extdata", "lh.aparc.annot.gz",
#'      package = "freesurferformats", mustWork = TRUE);
#'     annot = read.fs.annot(annotfile);
#'     colortable = colortable.from.annot(annot);
#'     head(colortable);
#'
#' @family atlas functions
#' @family colorLUT functions
#'
#' @export
colortable.from.annot <- function(annot, compute_colorcode=FALSE) {

  if(is.null(annot$colortable$table)) {
    stop("The annotation 'annot' must have a non-null annot$colortable$table matrix.");
  }

  colortable = annot$colortable;

  if(is.null(annot$colortable_df$struct_index)) {
    struct_index = seq(0, colortable$num_entries - 1);
  } else {
    struct_index = annot$colortable_df$struct_index;
  }

  colortable = data.frame("struct_index"=struct_index, "struct_name"=colortable$struct_names, "r"=colortable$table[,1], "g"=colortable$table[,2], "b"=colortable$table[,3], "a"=colortable$table[,4]);
  if(compute_colorcode) {
    colortable$code = colortable$r + colortable$g*2^8 + colortable$b*2^16 + colortable$a*2^24;
  }
  return(invisible(colortable));
}


