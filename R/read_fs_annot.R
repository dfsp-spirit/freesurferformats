#' @title Read file in FreeSurfer annotation format
#'
#' @description Read a data annotation file in FreeSurfer format. Such a file assigns a label and a color to each vertex of a brain surface. The assignment of labels to vertices is based on at atlas or brain parcellation file. Typically the atlas is available for some standard template subject, and the labels are assigned to another subject by registering it to the template.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/label/lh.aparc.annot', which contains the annotation based on the Desikan-Killiany Atlas for the left hemisphere of bert.
#'
#' @param filepath string. Full path to the input annotation file. Note: gzipped files are supported and gz format is assumed if the filepath ends with ".gz".
#'
#' @param empty_label_name character string, a base name to use to rename regions with empty name in the label table. This should not occur, and you can ignore this parameter setting. A warning will be thrown if this ever triggers. Not to be confused with parameter \code{default_label_name}, see below.
#'
#' @param metadata named list of arbitrary metadata to store in the instance.
#'
#' @param default_label_name character string, the label name to use for vertices which have a label code that does not occur in the label table. This is typically the case for the 'unknown' region, which often has code \code{0}. You can set this to avoid empty region label names. The typical setting would be 'unknown', however by default we leave the names as-is, so that annots which are read and then written back to files with this library do not differ.
#'
#' @return named list, entries are: "vertices" vector of n vertex indices, starting with 0. "label_codes": vector of n integers, each entry is a color code, i.e., a value from the 5th column in the table structure included in the "colortable" entry (see below). "label_names": the n brain structure names for the vertices, already retrieved from the colortable using the code. "hex_colors_rgb": Vector of hex color for each vertex.
#'      The "colortable" is another named list with 3 entries: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code. "colortable_df": The same information as a dataframe. Contains the extra columns "hex_color_string_rgb" and "hex_color_string_rgba" that hold the color as an RGB(A) hex string, like "#rrggbbaa".
#'
#' @family atlas functions
#'
#' @examples
#'     annot_file = system.file("extdata", "lh.aparc.annot.gz",
#'                                package = "freesurferformats",
#'                                mustWork = TRUE);
#'     annot = read.fs.annot(annot_file);
#'     print(annot);
#'
#' @importFrom grDevices rgb
#' @export
read.fs.annot <- function(filepath, empty_label_name="empty", metadata=list(), default_label_name="") {

    if(! file.exists(filepath)) {
      stop(sprintf("Annotation file '%s' does not exist or cannot be read.\n", filepath));
    }

    if(guess.filename.is.gzipped(filepath)) {
        fh = gzfile(filepath, "rb");
    } else {
        fh = file(filepath, "rb");
    }
    on.exit({ close(fh) }, add=TRUE);

    num_verts_and_labels = readBin(fh, integer(), n = 1, endian = "big");
    verts_and_labels = readBin(fh, integer(), n = num_verts_and_labels*2, endian = "big");

    verts = verts_and_labels[seq(1L, length(verts_and_labels), 2L)];
    labels = verts_and_labels[seq(2L, length(verts_and_labels), 2L)];

    return_list = list("vertices" = verts, "label_codes" = labels, "metadata"=metadata);

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
                stop(sprintf("Unsupported annotation file version '%d'.\n", version));   # nocov
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
        colortable_df$struct_index = colortable$struct_index;
        return_list$colortable_df = colortable_df;

        label_names = rep(default_label_name, length(labels));
        hex_colors_rgb = rep("#333333", length(labels));
        nempty = 1L;  # There could be more than 1 empty region, and we cannot match all of them to the same name. Ususally there should not be any labels with empty name though.
        for (i in seq_along(colortable$struct_names)) {
          label_code = code[i];
          label_name = colortable$struct_names[i];
          if(nchar(label_name) == 0) {
            warning(sprintf("Replacing empty label name with '%s'\n", empty_label_name));
            label_name = paste(empty_label_name, nempty, sep="");
            nempty = nempty + 1L;
          }
          hex_color_string_rgb = grDevices::rgb(colortable$table[i,1]/255., colortable$table[i,2]/255., colortable$table[i,3]/255.);
          label_names[labels==label_code] = label_name;
          hex_colors_rgb[labels==label_code] = hex_color_string_rgb;
        }
        return_list$label_names = label_names;
        return_list$hex_colors_rgb = hex_colors_rgb;

    }
    class(return_list) = c("fs.annot", class(return_list));
    return(return_list);
}


#' @title Print description of a brain atlas or annotation.
#'
#' @param x brain surface annotation or atlas with class `fs.annot`.
#'
#' @param ... further arguments passed to or from other methods
#'
#' @export
print.fs.annot <- function(x, ...) { # nocov start
  if(is.null(x$colortable)) {
    print(sprintf("Brain surface annotation without colortable for %d vertices containing %d unique region codes.", length(x$vertices), length(unique(x$label_codes))));
  } else {
    cat(sprintf("Brain surface annotation assigning %d vertices to %d brain regions.\n", length(x$vertices), nrow(x$colortable_df)));
    for(region_idx in seq_len(nrow(x$colortable_df))) {
      struct_index = region_idx;
      if(! is.null(x$colortable_df$struct_index)) {
        struct_index = x$colortable_df$struct_index[region_idx];
      }
      cat(sprintf(" - region #%d '%s': size %d vertices\n", struct_index, as.character(x$colortable_df$struct_name[[region_idx]]), sum(x$label_codes == x$colortable_df$code[[region_idx]])));
    }
  }
} # nocov end


#' @title Check whether object is an fs.annot
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is a brain surface annotation (that is, has "fs.annot" amongst its classes) and FALSE otherwise.
#'
#' @export
is.fs.annot <- function(x) inherits(x, "fs.annot")


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
  #dev_filepath = readChar(fh, num_chars_dev_filepath);
  seek(fh, where=num_chars_dev_filepath, origin='current'); # not needed, so we skip it to avoid warnings about NULL bytes.

  colortable$struct_names = rep("", ctable_num_entries);
  colortable$table = matrix(0, nrow = ctable_num_entries, ncol = 5);

  for (i in 1:ctable_num_entries) {
    num_chars_struct_name = readBin(fh, integer(), n = 1, endian = "big");

    struct_name = suppressWarnings(readChar(fh, num_chars_struct_name));  # Supress warning about included and ignroed NULL byte.

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
    #ctab_orig_dev_filename = readChar(fh, ctab_orig_dev_filename_length);
    seek(fh, where=ctab_orig_dev_filename_length, origin='current'); # not needed, so we skip it to avoid warnings about NULL bytes.

    colortable$struct_names = rep("", ctable_num_entries);
    colortable$table = matrix(0, nrow = ctable_num_entries, ncol = 5);

    # There is another field here which also encodes the number of entries.
    ctable_num_entries_2nd = readBin(fh, integer(), n = 1, endian = "big");
    if(ctable_num_entries != ctable_num_entries_2nd) {
      warning(sprintf("Meta data on number of color table mismatches: %d versus %d.\n", ctable_num_entries, ctable_num_entries_2nd));   # nocov
    }

    struct_identifier = rep(NA, ctable_num_entries); # the IDs of the regions in the file.
    for (i in seq_len(ctable_num_entries)) {
        struct_idx = i; # our internal index
        current_struct_identifier = readBin(fh, integer(), n = 1, endian = "big"); # the region identifier field in the file.
        struct_identifier[struct_idx] = current_struct_identifier;

        # Index must not be negative:
        if (current_struct_identifier < 0L) {
            stop(sprintf("Invalid struct index in color table entry #%d: index must not be negative but is '%d'.\n", i, current_struct_identifier));   # nocov
        }

        name_so_far = colortable$struct_names[struct_idx];
        # The same structure must not occur more than once (so the name should still be the empty string from the initialization when setting it):
        if (!identical(name_so_far, "")) {
            warning(sprintf("Annotation file entry #%d struct index %d: entry with identical name '%s' already hit, this must not happen. Brain structure names must be unique.\n", i, struct_idx, name_so_far)); # nocov
        }
        entry_num_chars = readBin(fh, integer(), n = 1, endian = "big");

        brain_structure_name = suppressWarnings(readChar(fh, entry_num_chars));

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
        colortable$struct_index = struct_identifier;
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


#' @title Read an annotation or label in GIFTI format.
#'
#' @param filepath string. Full path to the input label file in GIFTI format.
#'
#' @param element_index positive integer, the index of the dataarray to return. Ignored unless the file contains several dataarrays.
#'
#' @param labels_only logical, whether to ignore the colortable and region names. The returned annotation will only contain the a vector that contains one integer label per vertex (as entry 'label_codes'), but no region names and colortable information.
#'
#' @param rgb_column_names vector of exactly 4 character strings, order is important. The column names for the red, green, blue and alpha channels in the lable table. If a column does not exist, pass NA. If you do not know the column names, just call the function, it will print them. See 'labels_only' if you do not care.
#'
#' @param key_column_name character string, the column name for the key column in the lable table. This is the column that holds the label value from the raw vector (see 'labels_only') that links a label value to a row in the label table. Without it, one cannot recostruct the region name and color of an entry. Passing NA has the same effect as setting 'labels_only' to TRUE.
#'
#' @inheritParams read.fs.annot
#'
#' @family gifti readers
#'
#' @export
#' @importFrom xml2 xml_find_all read_xml xml_text
read.fs.annot.gii <- function(filepath, element_index=1L, labels_only=FALSE, rgb_column_names = c('Red', 'Green', 'Blue', 'Alpha'), key_column_name = 'Key', empty_label_name="unknown") {

  if(length(rgb_column_names) != 4L) {
    stop("Parameter 'rgb_column_names' must have length 4. Hint: Pass NA for values which are not available.");
  }

  if (requireNamespace("gifti", quietly = TRUE)) {
    gii = gifti::read_gifti(filepath);
    intent = gii$data_info$Intent[[element_index]];
    if(intent != 'NIFTI_INTENT_LABEL') {
      warning(sprintf("The intent of the gifti file is '%s', expected 'NIFTI_INTENT_LABEL'.\n", intent));  # nocov
    }
    if(is.null(gii$label)) {
      stop(sprintf("The gifti file '%s' does not contain label information.\n", filepath));   # nocov
    } else {

      #label_data_num_columns = ncol(gii$data[[element_index]]); # must be 1D for surface labels: 1 column of vertex indices (the data is returned as a matrix).
      if(gii$data_info$Dimensionality != 1L) {
        stop(sprintf("Label data has %d dimensions, expected 1. This does not look like a 1D surface label.\n", gii$data_info$Dimensionality));  # nocov
      }

      labels = as.integer(gii$data[[element_index]]); # note that as.integer() turns the (1 column) matrix into a vector.

      return_list = list("vertices" = seq.int(length(labels)), "label_codes" = labels, "metadata"=list());
      class(return_list) = c("fs.annot", class(return_list));

      if(labels_only | is.na(key_column_name)) {
        return(return_list);
      }

      num_regions_in_annot = nrow(gii$label);
      colortable_raw = as.data.frame(gii$label, stringsAsFactors = FALSE);
      colortable = list();
      colortable$table = matrix(rep(0.0, num_regions_in_annot * 5L), ncol = 5L);
      colnames(colortable$table) = c('r', 'g', 'b', 'a', 'code');

      missing_columns = c();
      colortable_column_index=1L;
      for(coln in rgb_column_names) {
        if(! is.na(coln)) {
          if(coln %in% colnames(colortable_raw)) {
            colortable$table[,colortable_column_index] = as.double(colortable_raw[[coln]]);
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
      colortable$struct_names = xml2::xml_text(label_nodes);

      if(key_column_name %in% colnames(colortable_raw)) {
        colortable$table[,5] = as.integer(colortable_raw[[key_column_name]]);
      } else {
        # Print the contents of the file to help the user fix the parameters.
        # Different software packages may have different names for the key column, and there is no way
        # to tell automatically which one it is. Therefore, we print all columns here, so the user
        # can identify a suitable one (if any).
        # We could do some wild guessing (like try the first integer column), but I don't like that idea too much.
        print("Label table cdata (region names for the rows below):");
        print(colortable$struct_names);
        print("Label table attribute columns:");
        print(colortable_raw);
        stop(sprintf("Specified key column '%s' does not exist in lable table attribute columns (available columns: '%s'). Fix parameter 'key_column_name'.\n", key_column_name, paste(colnames(colortable_raw), collapse = ", ")));
      }

      r = colortable$table[,1];
      g = colortable$table[,2];
      b = colortable$table[,3];
      a = colortable$table[,4];
      code = colortable$table[,5];

      max_color_value = 1.0;
      if(max(r) > 1.1) { # colors are in range 0-255
        max_color_value = 255;
      }
      hex_color_string_rgb = grDevices::rgb(r, g, b, maxColorValue = max_color_value);
      hex_color_string_rgba = grDevices::rgb(r, g, b, a, maxColorValue = max_color_value);

      colortable_df = data.frame(colortable$struct_names, r, g, b, a, code, hex_color_string_rgb, hex_color_string_rgba, stringsAsFactors = FALSE);
      colnames(colortable_df) = c("struct_name", "r", "g", "b", "a", "code", "hex_color_string_rgb", "hex_color_string_rgba");

      return_list$colortable = colortable;
      return_list$colortable_df = colortable_df;

      label_names = rep("", length(labels));
      hex_colors_rgb = rep("#333333", length(labels));
      nempty = 1L;  # There could be more than 1 empty region, and we cannot match all of them to the same name. Ususally there should not be any labels with empty name though.
      for (i in 1:length(colortable$struct_names)) {
        label_code = code[i];
        label_name = colortable$struct_names[i];
        hex_color_string_rgb = grDevices::rgb(colortable$table[i,1]/255., colortable$table[i,2]/255., colortable$table[i,3]/255.);
        if(nchar(empty_label_name) > 0 && nchar(label_name) == 0) {
          warning(sprintf("Replacing empty label name with '%s'\n", empty_label_name));
          label_name = paste(empty_label_name, nempty, sep="");
          nempty = nempty + 1L;
        }
        label_names[labels==label_code] = label_name;
        hex_colors_rgb[labels==label_code] = hex_color_string_rgb;
      }
      return_list$label_names = label_names;
      return_list$hex_colors_rgb = hex_colors_rgb;
      return(return_list);
    }

  } else {
    stop("The 'gifti' package must be installed to use this functionality.");    # nocov
  }
}


#' @title Read FreeSurfer GCA file.
#'
#' @param filepath character string, path to a file in binary GCA format. Stores array of Gaussian classifiers for probabilistic atlas.
#'
#' @return named list, the file fields. The GCA data is in the data field.
#'
#' @author This function is based on Matlab code by Bruce Fischl, published under the FreeSurfer Open Source License available at \url{https://surfer.nmr.mgh.harvard.edu/fswiki/FreeSurferSoftwareLicense}. The R version was written by Tim Schaefer.
#'
#' @examples
#' \dontrun{
#' gca_file = file.path(Sys.getenv('FREESURFER_HOME'), 'average', 'face.gca');
#' gca = read.fs.gca(gca_file);
#' }
#'
#' @export
read.fs.gca <- function(filepath) {
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add = TRUE);
  endian = "big";
  ret_list = list();

  gca_num_mrf = 1L;
  max_labels = 4L;
  gibbs_neighborhood_size = 6L;

  ret_list$gca_version = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
  if(ret_list$gca_version < 1.0 | ret_list$gca_version > 4.0) {
    stop(sprintf("Invalid GCA file format version in file header: %f\n", ret_list$gca_version));
  }

  ret_list$prior_spacing = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
  ret_list$node_spacing = readBin(fh, numeric(), size = 4, n = 1, endian = endian);

  prior_width = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  prior_height = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  prior_depth = readBin(fh, integer(), size = 4, n = 1, endian = endian);

  node_width = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  node_height = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  node_depth = readBin(fh, integer(), size = 4, n = 1, endian = endian);

  ret_list$num_inputs = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  flags = readBin(fh, integer(), size = 4, n = 1, endian = endian);

  gca = matrix(nrow = (prior_width * prior_height * prior_depth), ncol = (2L * max_labels + 1L));
  #cat(sprintf("Prior dimension: %d x %d x %d, spacing=%d, version=%f\n", prior_width, prior_height, prior_depth, ret_list$prior_spacing, ret_list$gca_version));
  #cat(sprintf("Node dimension: %d x %d x %d, num_inputs=%d, flags=%d\n", node_width, node_height, node_depth, ret_list$num_inputs, flags));
  #cat(sprintf("GCA dimension: %d x %d\n", dim(gca)[1], dim(gca)[2]));

  gca_row_idx = 1L;
  for(idx_x in seq.int(node_width)) {
    #cat(sprintf("[Node] Reading slice %d of %d.\n", idx_x, node_width));
    for(idx_y in seq.int(node_height)) {
      for(idx_z in seq.int(node_depth)) {
        num_labels = readBin(fh, integer(), size = 4, n = 1, endian = endian);
        total_training = readBin(fh, integer(), size = 4, n = 1, endian = endian);
        gca[gca_row_idx, 1L] = num_labels;

        if(num_labels > 0L) {
          for(label_idx in seq.int(num_labels)) {
            label = readBin(fh, integer(), size = 1, n = 1, signed = FALSE, endian = endian);
            lmean = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
            lmean = NULL;     # not used
            lvar = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
            lvar = NULL;     # not used
            if(bitwAnd(flags, gca_num_mrf)) {
              next;
            }
            for(gibbs_idx in seq.int(gibbs_neighborhood_size)) {
              num_gibbs_labels = readBin(fh, integer(), size = 4, n = 1, endian = endian);
              for(gibbs_label_idx in seq.int(num_gibbs_labels)) {
                gibbs_label = readBin(fh, integer(), size = 4, n = 1, endian = endian); # TODO: must be uint32
                gibbs_label = NULL;  # not used
                gibbs_prior = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
                gibbs_prior = NULL;  # not used
              }
            }

          }
        }
        gca_row_idx = gca_row_idx + 1L;
      }
    }
  }

  gca_row_idx = 1L;
  for(idx_x in seq.int(prior_width)) {
    #cat(sprintf("[Prior] Reading slice %d of %d.\n", idx_x, prior_width));
    for(idx_y in seq.int(prior_height)) {
      for(idx_z in seq.int(prior_depth)) {
        num_labels = readBin(fh, integer(), size = 4, n = 1, endian = endian);
        total_training = readBin(fh, integer(), size = 4, n = 1, endian = endian);
        gca[gca_row_idx, 1L] = num_labels;

        if(num_labels > 0L) {
          for(label_idx in seq.int(num_labels)) {
            label = readBin(fh, integer(), size = 1, n = 1, signed = FALSE, endian = endian);
            prior = readBin(fh, numeric(), size = 4, n = 1, endian = endian);

            if(label_idx <= max_labels) {
              gca[gca_row_idx, (2L * label_idx)] = label;
              gca[gca_row_idx, (2L * label_idx + 1L)] = prior;
            }

          }
        }
        gca_row_idx = gca_row_idx + 1L;
      }
    }
  }

  ret_list$data = gca;
  return(ret_list);
}


#' @title Get max region index of an fs.annot instance.
#'
#' @param annot fs.annot instance
#'
#' @return integer, the max region index. They typically start with 0 and are consecutive, but this is not enforced or checked in any way.
#'
#' @note This is a helper function to be used with \code{annot.unique}, see the example there.
#'
#' @export
annot.max.region.idx <- function(annot) {
  if(is.null(annot$colortable_df$struct_index)) {
    return(nrow(annot$colortable_df));
  } else {
    return(max(annot$colortable_df$struct_index));
  }
}




# lh_annot = read.fs.annot("~/data/tim_only/tim/label/lh.aparc.annot");
# rh_annot = read.fs.annot("~/data/tim_only/tim/label/rh.aparc.annot");
# rh_annot_mod = annot.unique(...
# write.fs.annot("~/test.annot", annot=rh_annot_mod);
# To test whether the annot is valid and freesurfer can read it, try converting with mris_convert:
# $ mris_convert --annot ~/test.annot data/tim_only/tim/surf/rh.white rh.aparc.gii

#' @title Make the region names and indices unique across hemispheres for a parcellation.
#'
#' @description Sometimes you need an annotation to use unique IDs and region names across hemispheres, but that is not the case for the standard FreeSurfer parcellations. So what you need to do is change the codes and names for one hemi. Typically the left hemi annot will be left as is, and the right hemi annot will be modified using this function.
#'
#' @param annot the annot in which to change the ids and names.
#'
#' @param add_to_region_indices integer, a single value to add to the region indices. This is typically equal to the number of regions in the left hemisphere plus one (e.g., 36+1=37 for the 'aparc' atlas), as the region indices typically start at 0 and are consecutive, but you may want to check the maximal region id of the left hemi is in doubt. Pass \code{0} to leave the IDs intact.
#'
#' @param region_name_prefix character string, a prefix to modify the region names to make them unique. Pass `NULL` if you do not want a prefix.
#'
#' @param region_name_suffix character string, a suffix to modify the region names to make them unique.  Pass `NULL` if you do not want a suffix.
#'
#' @param set_first_idx_zero logical, whether to apply special treatment to first region (the 'unknown' region) in annot and set its ID to \code{0}.
#'
#' @examples
#' \dontrun{
#' lh_annot = read.fs.annot("~/data/study1/subject1/label/lh.aparc.annot");
#' lh_annot; # shows info including region IDs
#' rh_annot = read.fs.annot("~/data/study1/subject1/label/rh.aparc.annot");
#' rh_annot_mod = annot.unique(rh_annot, annot.max.region.idx(lh_annot)+1L, region_name_prefix='rh_');
#' }
#'
#' @note This function is not part of the official API and should not be used. It is currently broken.
#'
#' @keywords internal
annot.unique <- function(annot, add_to_region_indices, region_name_prefix="rh_", region_name_suffix=NULL, set_first_idx_zero=FALSE) {
  if(! is.fs.annot(annot)) {
    stop("Parameter 'annot' must be an fs.annot instance.");
  }
  if(! is.integer(add_to_region_indices)) {
    stop("Parameter 'add_to_region_indices' must be of type integer.");
  }

  ### Adapt region indices. ###
  # First make sure there is a struct index in the colortable, add if not.
  if(is.null(annot$colortable_df$struct_index)) {
    annot$colortable_df$struct_index = seq(0, nrow(annot$colortable_df) - 1) + 1L;
  }
  # Now modify the struct_index as requested.
  annot$colortable_df$struct_index = annot$colortable_df$struct_index + add_to_region_indices;
  if(set_first_idx_zero) {
    annot$colortable_df$struct_index[1] = 0L;
  }
  annot$colortable$struct_index = annot$colortable_df$struct_index;

  ### Add prefix/suffix to names. ###
  if(! is.null(region_name_prefix)) {
    annot$label_names = paste(region_name_prefix, annot$label_names, sep = "");
    annot$colortable$struct_names = paste(region_name_prefix, annot$colortable$struct_names, sep = "");
    annot$colortable_df$struct_name = paste(region_name_prefix, annot$colortable_df$struct_name, sep = "");
  }
  if(! is.null(region_name_suffix)) {
    annot$label_names = paste(annot$label_names, region_name_suffix, sep = "");
    annot$colortable$struct_names = paste(annot$colortable$struct_names, region_name_suffix, sep = "");
    annot$colortable_df$struct_name = paste(annot$colortable_df$struct_name, region_name_suffix, sep = "");
  }
  return(annot);
}
