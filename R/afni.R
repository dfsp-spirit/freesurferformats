# Some basic functions for parsing a tiny subset of the AFNI file formats.


#' @title Get tree of elements in AFNI dset file.
#'
#' @param filepath character string, path to a file in AFNI dset format.
#'
#' @return a tree
#'
#' @examples
#'     afni.tree("~/AFNI_demos/FATCAT_DEMO/SUMA/std.60.lh.aparc.a2009s.annot.niml.dset");
#'     afni.tree("~/AFNI_demos/AFNI_InstaCorrDemo.mini/srf/rest_sub00440.BEZ_lh_SSM.std.60.niml.dset"); # with binary data, not supported yet due to invalid line breaks when reading as ASCII text.
#'
#' @export
afni.tree <- function(filepath, verbose = TRUE) {
  if(! file.exists(filepath)) {
    stop(sprintf("Cannot read AFNI dset file '%s'.\n", filepath));
  }
  current_node_idx = 1L; # assigned sequentially to new nodes
  current_line_number = 0L;
  current_depth = 0L;
  current_name = paste("node_", current_node_idx, '_root', sep = "");
  root = data.tree::Node$new(current_name);
  root$node_idx = current_node_idx;
  current_node = root;
  parent = NULL; # TODO: store parents on a stack
  in_data = FALSE;
  con = file(filepath, "r");
  while(TRUE) {
    line = readLines(con, n = 1L);
    #if(verbose) {
    #  cat(sprintf("Handling line %d of %d: '%s'.\n", line_idx, length(all_lines), line));
    #}
    if(length(line) == 0L) {
      break;
    } else {
      current_line_number = current_line_number + 1L;
      if(startsWith(line, "<") & !(startsWith(line, "</"))) {
        current_node_idx = current_node_idx + 1L;
        new_node_type = substring(line, 2L);
        new_node_name = paste("node_", current_node_idx, '_', new_node_type, sep = "");
        current_depth = current_depth + 1L;
        if(verbose) {
          cat(sprintf("Creating node '%s' (#%d) of type '%s' as child of '%s' based on line %d (depth is now %d).\n", new_node_name, current_node_idx, new_node_type, current_node$name, current_line_number, current_depth));
        }

        new_node = current_node$AddChild(new_node_name);
        new_node$node_idx = current_node_idx;
        parent = current_node;
        current_node = new_node;
      } else if(startsWith(line, "</")) {
        if(is.null(parent)) {
          stop("Invalid dset file: no parent left."); # must never happen, we have our own root node.
        }
        current_depth = current_depth - 1L;
        if(current_depth < 0L) {
          stop("Invalid dset file: reached negative node depth."); # must never happen
        }
        if(verbose) {
          cat(sprintf("Definition of node '%s' (#%d) finished at line %d, returning to parent '%s' (depth is now %d).\n", current_node$name, current_node$node_idx, current_line_number, parent$name, current_depth));
        }
        if(in_data) {
          cat(sprintf(" -Closing data section of node '%s' (#%d) at line %d.\n", current_node$name, current_node$node_idx, current_line_number));
          in_data = FALSE;
        } else {
          cat(sprintf(" -Not in data section at end of node '%s' (#%d) at line %d.\n", current_node$name, current_node$node_idx, current_line_number));
        }
        current_node = parent;
      } else if(endsWith(line, ">")) {
        cat(sprintf(" -New subsection of node '%s' (#%d) encountered at line %d ('%s'), maybe data. Data starts in next line.\n", current_node$name, current_node_idx, current_line_number, line));
        in_data = TRUE;
        # TODO: revert connection to start of data (the current line may already include the start of the data after the '>') and read the data -- maybe in binary form.

      } else if(grepl( ">", line, fixed = TRUE)) {
        cat(sprintf(" -New subsection of node '%s' (#%d) encountered at line %d ('%s'), maybe data. Data starts in this line.\n", current_node$name, current_node_idx, current_line_number, line));
        in_data = TRUE;
        # TODO: revert connection to start of data (the current line may already include the start of the data after the '>') and read the data -- maybe in binary form.

        search_start = TRUE;
        if(search_start) {

          cur_pos = seek(con, where=NA);
          cur_char = "?";

          seek(con, where = -1L, origin = "current");
          while(cur_char != ">" & cur_pos > 0L) {
            cur_char = readChar(con, 1L, useBytes = TRUE);
            if(length(cur_char) == 0L) {
              cat(sprintf(" --Start of data section of node %s NOT found, cannot read more chars.\n", current_node$name));
              break;
            }
            seek(con, where = -2L, origin = "current");
            cur_pos = seek(con, where=NA);
            cat(sprintf("Searching for data start of node %s, currently at byte %d which contains char %s.\n", current_node$name, cur_pos, cur_char));
          }
          if(length(cur_char) == 0L) {
            cat(sprintf(" --Start of data section of node %s NOT found, hit file start.\n", current_node$name));
          } else {
            if(cur_char == ">") {
              cat(sprintf(" --Start of data section of node %s is at byte %d in file.\n", current_node$name, cur_pos));
            } else {
              cat(sprintf(" --Start of data section of node %s NOT found.\n", current_node$name));
            }
          }
        }

      }
    }
  }

  close(con);
  return(root);
}
