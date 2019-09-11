#' @title Aggregate morphometry data over brain atlas regions for a subject.
#'
#' @description Aggregate morphometry data over brain atlas regions, e.g., compute the mean thickness value over all regions in an atlas.
#'
#' @param vertex_morph_data, numeric vector. The morphometry data, one value per vertex. The morphometry data are typically loaded from an MGZ or curv format file with the read.fs.curv or read.fs.mgh functions.
#'
#' @param vertex_label_names, string vector. The region names for the vertices, one string per vertex. The region names are typically loaded from an annotation file with the read.fs.annot function.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @param requested_label_names, string vector. The label (or region) names that you want to occur in the output. If not specified, all region names which occur in the data are used. If given, and one of the requested names does NOT occur in the data, it will occur in the output with aggregation value NaN. If given, and one of the names from the data does NOT occur in the requested list, it will NOT occur in the output. So if you specify this, the output dataframe will contain a row for a region if and only if it is in the requested list.
#'
#' @return dataframe with aggregated values for all regions, with 2 columns and n rows, where n is the number of effective regions. The columns are: "region": string, contains the region name. "aggregated": numeric, contains the result of applying agg_fun to the morphometry data in that region.
#'
#'
#' @keywords internal
fs.atlas.region.agg <- function(vertex_morph_data, vertex_label_names, agg_fun = mean, requested_label_names = c()) {

  if (length(vertex_morph_data) != length(vertex_label_names)) {
      stop(sprintf("Data mismatch: Reveived morphometry data for %d vertices, but %d labels. Counts must match.\n", length(vertex_morph_data), length(vertex_label_names)));
  }

  if (!length(requested_label_names)) {
      requested_label_names = unique(vertex_label_names);
      did_request_regions = FALSE;
  } else {
      did_request_regions = TRUE;
  }

  df = data.frame("vertex_morph_data"=vertex_morph_data, "vertex_label_names"=vertex_label_names);
  agg = aggregate(df$vertex_morph_data, by=list(df$vertex_label_names), FUN=agg_fun, drop = FALSE);
  colnames(agg) = c("region", "aggregated");

  if (did_request_regions) {
      # Add explicitely requested regions for which no data was found as rows to the dataframe (with aggregation value NA).
      for(possible_region in requested_label_names) {
          if(! possible_region %in% agg$region) {
              agg<-rbind(agg, data.frame("region"=possible_region, "aggregated"=NaN))
          }
      }

      # Delete data for regions which are not in the list of requested region names.
      agg = agg[ (agg$region %in% requested_label_names), ];
  }

  return(agg);
}

#' @title Aggregate morphometry data over brain atlas regions and subjects for a group of subjects.
#'
#' @description Aggregate morphometry data over brain atlas regions, e.g., compute the mean thickness value over all regions in an atlas for all subjects.
#'
#' @param subjects_dir, string. The FreeSurfer SUBJECTS_DIR, i.e., a directory containing the data for all your subjects, each in a subdir named after the subject identifier.
#'
#' @param subjects_list, string vector. A vector of subject identifiers that match the directory names within subjects_dir.
#'
#' @param measure, string. Name of the vertex-wise measure of morphometry data file. E.g., "area" or "thickness". Used to construct the name of the morphometry file to be loaded.
#'
#' @param hemi, string, one of 'lh' or 'rh'. The hemisphere name. Used to construct the names of the annotation and morphometry data files to be loaded.
#'
#' @param atlas, string. The atlas name. E.g., "aparc", "aparc.2009s", or "aparc.DKTatlas". Used to construct the name of the annotation file to be loaded.
#'
#' @param agg_fun, function. An R function that aggregates data, typically max, mean, min or something similar. Note: this is NOT a string, put the function name without quotes. Defaults to mean.
#'
#' @return dataframe with aggregated values for all regions and subjects, with n columns and m rows, where n is the number of subjects and m is the number of regions.
#'
#'
#' @keywords internal
fs.atlas.region.agg.group <- function(subjects_dir, subjects_list, measure, hemi, atlas, agg_fun = mean) {
    if (! dir.exists(subjects_dir)) {
        stop(sprintf("Subjects directory '%s' does not exist or cannot be accessed.\n", subjects_dir));
    }

    agg_all_subjects = data.frame()
    for (subject_id in subjects_list) {
        curvfile = file.path(subjects_dir, subject_id, "surf", sprintf("%s.%s", hemi, measure));
        morph_data = read.fs.curv(curvfile);

        annot_file = file.path(subjects_dir, subject_id, "label", sprintf("%s.%s.annot", hemi, atlas));
        annot = read.fs.annot(annot_file);

        subject_agg = fs.atlas.region.agg(morph_data, annot$label_names, agg_fun=agg_fun)
        subject_agg$subject = subject_id;

        if(nrow(agg_all_subjects) > 0) {
          agg_all_subjects = rbind(agg_all_subjects, subject_agg)
        } else {
          agg_all_subjects = subject_agg;
        }
    }
    agg_res = reshape::cast(agg_all_subjects, subject~region, value='aggregated');
    rownames(agg_res) = subjects_list;
    return(as.data.frame(agg_res));
}
