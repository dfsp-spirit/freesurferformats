#' @title Read file in FreeSurfer annotation format
#'
#' @description Read a data annotation file in FreeSurfer format. Such a file assigns a label and a color to each vertex of a brain surface. The assignment of labels to vertices is based on at atlas or brain parcellation file. Typically the atlas is available for some standard template subject, and the labels are assigned to another subject by registering it to the template.
#'    For a subject (MRI image pre-processed with FreeSurfer) named 'bert', an example file would be 'bert/label/lh.aparc.annot', which contains the annotation based on the Desikan-Killiany Atlas for the left hemisphere of bert.
#'
#' @param filepath, string. Full path to the input curv file.
#'
#' @return named list, enties are: "vertices" vector of n vertex indices, starting with 0. "label_codes": vector of n integers, each entry is a color code, i.e., a value from the 5th column in the table structure included in the "colortable" entry (see below). "label_names": the n brain structure names for the vertices, already retrieved from the colortable using the code.
#'      The "colortable" is another named list with 3 entries: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code.
#'
#'
#' @export
read.fs.annot <- function(filepath) {

    fh = file(filepath, "rb");

    num_verts_and_labels = readBin(fh, integer(), n = 1, endian = "big");
    cat(sprintf("\\nnum_verts_and_labels = %d.\n", num_verts_and_labels));
    verts_and_labels = readBin(fh, integer(), n = num_verts_and_labels*2, endian = "big");

    verts = verts_and_labels[seq(1L, length(verts_and_labels), 2L)];
    labels = verts_and_labels[seq(2L, length(verts_and_labels), 2L)];

    return_list = list("vertices" = verts, "label_codes" = labels);

    cat(sprintf("Loaded %d verts, %s labels.\n", length(verts), length(labels)));

    has_colortable = readBin(fh, logical(), n = 1, endian = "big");
    cat(sprintf("has_colortable is: %d.\n", has_colortable));

    if (has_colortable) {
        ctable_num_entries = readBin(fh, integer(), n = 1, endian = "big");

        if(ctable_num_entries > 0) {
            stop(sprintf("Unsupported old annotation file version.", version));
        } else {
            # If ctable_num_entries is negative, it is a version code (actually, the abs value is the version).
            version = -ctable_num_entries;
            cat(sprintf("Colortable has negative number of entries (%d), version set to abs value %d.\n", ctable_num_entries, version));
            if(version == 2) {
                ctable_num_entries = readBin(fh, integer(), n = 1, endian = "big");

                colortable = readcolortable(fh, ctable_num_entries);

                cat(sprintf("v2 colortable with %d entries read.\n", colortable$num_entries));
                return_list$colortable = colortable;

                struct_names = colortable$struct_names;
                r = colortable$table[,1];
                g = colortable$table[,2];
                b = colortable$table[,3];
                a = colortable$table[,4];
                code = colortable$table[,5];
                colortable_df = data.frame(struct_names, r, g, b, a, code)

                label_names = rep("", length(labels))
                for (i in 1:length(colortable$struct_names)) {
                    label_code = code[i]
                    cat(sprintf("Handling struct #%d named '%s' with code %d.\n", i, colortable$struct_names[i], label_code))
                    label_names[labels==label_code] = colortable$struct_names[i]
                }
                return_list$label_names = label_names;
            }
            else {
                stop(sprintf("Unsupported annotation file version '%d'.", version));
            }
        }

    }
    close(fh);
    return(return_list);
}





#' @title Read binary colortable in v2 format.
#'
#' @description Read a v2 format colortable from a connection to a binary file.
#'
#' @param filehandle: file handle
#'
#' @return named list: the color table. The named entries are: "num_entries": int, number of brain structures. "struct_names": vector of strings, the brain structure names. "table": numeric matrix with num_entries rows and 5 colums. The 5 columns are: 1 = color red channel, 2=color blue channel, 3=color green channel, 4=color alpha channel, 5=unique color code.
#'
#'
#' @keywords internal
readcolortable <- function(fh, ctable_num_entries) {

    cat(sprintf("readcolortable: Reading v2 colortable with %d entries.\n", ctable_num_entries));

    colortable <- list("num_entries" = ctable_num_entries)
    ctab_orig_dev_filename_length = readBin(fh, integer(), n = 1, endian = "big");

    # Orginial filename of the colortable file that was used to create the atlas colortable (on the dev machine).
    ctab_orig_dev_filename = readChar(fh, ctab_orig_dev_filename_length);
    cat(sprintf("Filename of dev version with %d chars was '%s'.\n", ctab_orig_dev_filename_length, ctab_orig_dev_filename));


    colortable$struct_names = rep("", ctable_num_entries);
    colortable$table = matrix(0, nrow = ctable_num_entries, ncol = 5);

    # There is another field here which also encodes the number of entries.
    ctable_num_entries_2nd = readBin(fh, integer(), n = 1, endian = "big");
    cat(sprintf("Read ctable_num_entries_2nd: %d\n", ctable_num_entries_2nd));
    for (i in 1:ctable_num_entries) {
        struct_idx = readBin(fh, integer(), n = 1, endian = "big") + 1;
        cat(sprintf("Reading entry #%d with struct_idx '%d'.\n", i, struct_idx));

        # Index must not be negative:
        if (struct_idx < 0) {
            stop(sprintf("Invalid struct index in color table entry #%d: index must not be negative but is '%d'.\n", i, struct_idx));
        } else {
            cat(sprintf("#%d struct_idx '%d' ok.\n", i, struct_idx));
        }

        name_so_far = colortable$struct_names[struct_idx];
        #cat(sprintf("#%d Name at index %d so far: '%s'\n", i, struct_idx, name_so_far));
        # The same structure must not occur more than once:
        if (!identical(name_so_far, "")) {
            cat(sprintf("Annotation file entry #%d struct index %d: entry with identical name '%s' already hit, this must not happen. Brain structure names must be unique.\n", i, struct_idx, name_so_far));
        }
        entry_num_chars = readBin(fh, integer(), n = 1, endian = "big");
        cat(sprintf("Reading entry #%d with %d chars.\n", i, entry_num_chars));

        brain_structure_name = readChar(fh, entry_num_chars);
        #brain_structure_name = brain_structure_name[1:length(brain_structure_name)-1];
        cat(sprintf("Reading entry #%d for brain structure '%s'.\n", i, brain_structure_name));

        colortable$struct_names[i] = brain_structure_name;

        r = readBin(fh, integer(), n = 1, endian = "big");   # red channel of color
        g = readBin(fh, integer(), n = 1, endian = "big");   # green channel of color
        b = readBin(fh, integer(), n = 1, endian = "big");   # blue channel of color
        a = readBin(fh, integer(), n = 1, endian = "big");   # alpha channel of color
        unique_color_label = r + g*2^8 + b*2^16 + a*2^24;

        cat(sprintf("Read RGBA color: (%d, %d, %d, %d), unique code is %d.\n", r, g, b, a, unique_color_label));

        colortable$table[i,1] = r;
        colortable$table[i,2] = g;
        colortable$table[i,3] = b;
        colortable$table[i,4] = a;
        colortable$table[i,5] = unique_color_label;
    }

    return(colortable);
}