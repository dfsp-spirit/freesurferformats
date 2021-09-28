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
        cat(sprintf(" -New subsection of node '%s' (#%d) encountered at line %d ('%s'), maybe data.\n", current_node$name, current_node_idx, current_line_number, line));
        in_data = TRUE;
      } else {

      }
    }
  }

  close(con);
  return(root);
}
