#' @title Aggregate morphometry data over brain atlas regions.
#'
#' @description Aggregate morphometry data over brain atlas regions, e.g., compute the mean thickness value over all regions in an atlas.
#'
#' @param filepath, string. Full path to the input curv file.
#'
#' @return dunno
#'
#'
#' @export
fs.atlas.region.agg <- function(vertex_morph_data, vertex_label_names, agg_fun = mean, all_possible_label_names = c()) {

  if (length(vertex_morph_data) != length(vertex_label_names)) {
      stop(sprintf("Data mismatch: Reveived morphometry data for %d vertices, but %d labels. Counts must match.\n", length(vertex_morph_data), length(vertex_label_names)));
  }

  if (!length(all_possible_label_names)) {
    all_possible_label_names = unique(vertex_label_names);
    did_request_regions = FALSE;
  } else {
    did_request_regions = TRUE;
  }

  df = data.frame("vertex_morph_data"=vertex_morph_data, "vertex_label_names"=vertex_label_names);
  agg = aggregate(df$vertex_morph_data, by=list(df$vertex_label_names), FUN=agg_fun, drop = FALSE);
  colnames(agg) = c("region", "aggregated");

  if (did_request_regions) {
      # Add explicitely requested regions for which no data was found as rows to the dataframe (with aggregation value NA).
      for(possible_region in all_possible_label_names) {
          if(! possible_region %in% agg$region) {
              agg<-rbind(agg, data.frame("region"=possible_region, "aggregated"=NaN))
          }
      }

      # Delete data for regions which are not in the list of requested region names.
      agg = agg[ (agg$region %in% all_possible_label_names), ];
  }


  return(agg);
}
