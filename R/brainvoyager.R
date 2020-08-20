

#' @title Read Brainvoyager statistical surface results from v3 SMP file.
#'
#' @inheritParams read.smp.brainvoyager
#'
#' @note Do not call this, call \code{read.smp.brainvoyager} instead, which will figure out the version and call the appropriate function.
#'
#' @return named list of file contents
#'
#' @keywords internal
read.smp.brainvoyager.v3 <- function(filepath) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);

  if(ret_list$smp_version != 3L) {
    stop(sprintf("Found SMP file in file format version %d, only version 3 is supported.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  for(map_index in seq.int(ret_list$num_maps)) {
    vm = list();
    vm$map_type = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$num_lags = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$min_lag = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$max_lag = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cc_overlay = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cluster_size = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$enable_cluster_check = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$stat_threshold_critical = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$stat_threshold_max = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_1_fnom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_2_fdenom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cortex_bonferroni_correct = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$color_critical_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$color_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$enable_smp_color = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$transparent_color_factor = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$map_name = readBin(fh, character(), n = 1);
    ret_list$vertex_maps[[map_index]] = vm;
  }

  # header done, now read the data
  for(map_index in seq.int(ret_list$num_maps)) {
    ret_list$vertex_maps[[map_index]]$data = readBin(fh, numeric(), size = 4, n = ret_list$num_mesh_vertices, endian = endian);
  }
  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}


#' @title Read Brainvoyager statistical surface results from v2 SMP file.
#'
#' @inheritParams read.smp.brainvoyager
#'
#' @note Do not call this, call \code{read.smp.brainvoyager} instead, which will figure out the version and call the appropriate function.
#'
#' @return named list of file contents
#'
#' @keywords internal
read.smp.brainvoyager.v2 <- function(filepath) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);

  if(ret_list$smp_version != 2L) {
    stop(sprintf("Found SMP file in file format version %d, only version 2 is supported.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$map_type = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$num_lags = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  for(map_index in seq.int(ret_list$num_maps)) {
    vm = list();
    vm$cluster_size = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$enable_cluster_check = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$stat_threshold_critical = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$stat_threshold_max = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_1_fnom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$degrees_of_freedom_2_fdenom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$cortex_bonferroni_correct = readBin(fh, integer(), size = 4, n = 1, endian = endian);
    vm$color_critical_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$color_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
    vm$enable_smp_color = readBin(fh, integer(), size = 1, n = 1, endian = endian);
    vm$transparent_color_factor = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
    vm$map_name = readBin(fh, character(), n = 1);
    ret_list$vertex_maps[[map_index]] = vm;
  }

  # header done, now read the data
  for(map_index in seq.int(ret_list$num_maps)) {
    ret_list$vertex_maps[[map_index]]$data = readBin(fh, numeric(), size = 4, n = ret_list$num_mesh_vertices, endian = endian);
  }
  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}


#' @title Read Brainvoyager statistical surface results from SMP file.
#'
#' @param filepath character string, path to file in Brainvoyager SMP file format
#'
#' @references see \url{https://support.brainvoyager.com/brainvoyager/automation-development/84-file-formats/40-the-format-of-smp-files} for the spec
#'
#' @note Currently only SMP file versions 2 and 3 are supported, as these are the only ones for which a spec is available. The version is encoded in the file header.
#'
#' @return named list of file contents
#'
#' @examples
#' \dontrun{
#'  # Surface mesh, requires BV demo dataset from website:
#'  sf = read.fs.surface.bvsrf("~/data/BrainTutorData/CG_LHRH_D65534.srf");
#'  # Surface map of cortical thickness. Needs to be created in BV.
#'  smp_file = "~/data/BrainTutorData/CG_LHRH_D65534_Thickness.smp";
#'  smp = read.smp.brainvoyager(smp_file);
#'  smp_data = read.fs.morph.bvsmp(smp); # could also pass smp_file.
#'  fsbrain::vis.fs.surface(sf, per_vertex_data = smp_data);
#' }
#'
#' @export
read.smp.brainvoyager <- function(filepath) {
  endian = "little";
  fh = file(filepath, "rb");
  smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  close(fh);

  if(smp_version == 3L) {
    return(read.smp.brainvoyager.v3(filepath));
  } else if(smp_version == 2L) {
    return(read.smp.brainvoyager.v2(filepath));
  } else {
    stop(sprintf("Found SMP file in file format version %d, only versions 2 and 3 are supported.\n", smp_version));
  }
}


#' @title Check whether object is a bvsmp instance.
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is an bvsmp instane (that is, has "bvsmp" amongst its classes) and FALSE otherwise.
#' @export
is.bvsmp <- function(x) inherits(x, "bvsmp")


#' @title Write a brainvoyager SMP file.
#'
#' @description Write a brainvoyager SMP file, which contains one or more vertex-wise data maps (stats or morphometry data).
#'
#' @param filepath character string, the output file
#'
#' @param bvsmp bvsmp instance, a named list as returned by \code{\link{read.smp.brainvoyager}}.
#'
#' @param smp_version integer, the SMP file format version to use when writing. Only v3 is supported.
#'
#' @seealso \code{\link{write.fs.morph.smp}}
#'
#' @export
write.smp.brainvoyager <- function(filepath, bvsmp, smp_version = 3L) {
  if(! is.bvsmp(bvsmp)) {
    stop("Parameter 'bvsmp' must contain bvsmp instance.\n");
  }
  if(smp_version == 3L) {
    return(write.smp.brainvoyager.v3(filepath, bvsmp));
  } else {
    stop(sprintf("Brainvoyager SMP file format version not supported, only version 3 is supported.\n"));
  }
}


#' @title Write a brainvoyager v3 SMP file.
#'
#' @inheritParams write.smp.brainvoyager
#'
#' @note Called by \code{\link{write.smp.brainvoyager}}.
#'
#' @keywords internal
write.smp.brainvoyager.v3 <- function(filepath, bvsmp) {
  endian = "little";
  fh = file(filepath, "wb", blocking = TRUE);

  smp_version = 3L;

  writeBin(as.integer(smp_version), fh, size = 2, endian = endian);
  writeBin(as.integer(bvsmp$num_mesh_vertices), fh, size = 4, endian = endian);
  writeBin(as.integer(bvsmp$num_maps), fh, size = 2, endian = endian);
  writeBin(bvsmp$srf_file_name, fh, endian = endian);
  if(bvsmp$num_maps > 0L) {
    for(vm in bvsmp$vertex_maps) {

      writeBin(as.integer(vm$map_type), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$num_lags), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$min_lag), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$max_lag), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$cc_overlay), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$cluster_size), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$enable_cluster_check), fh, size = 1, endian = endian);
      writeBin(as.double(vm$stat_threshold_critical), fh, size = 4, endian = endian);
      writeBin(as.double(vm$stat_threshold_max), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$degrees_of_freedom_1_fnom), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$degrees_of_freedom_2_fdenom), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$cortex_bonferroni_correct), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$color_critical_rgb), fh, size = 1, endian = endian); # color_critical_rgb is vector of length 3
      writeBin(as.integer(vm$color_max_rgb), fh, size = 1, endian = endian); # color_max_rgb is vector of length 3
      writeBin(as.integer(vm$enable_smp_color), fh, size = 1, endian = endian);
      writeBin(as.double(vm$transparent_color_factor), fh, size = 4, endian = endian);
      writeBin(as.character(bvsmp$map_name), fh, endian = endian);
    }

    for(vm in bvsmp$vertex_maps) { # write data
      writeBin(as.double(vm$data), fh, size = 4, endian = endian);
    }
  }
  close(fh);
}


#' @title Create new bvsmp instance encoding morph data for Brainvoyager.
#'
#' @param morph_data numeric vector, the morphometry data to store in the bvsmp instance (one value per mesh vertex).
#'
#' @return bvsmp instance, can be used to write Brainvoyager SMP format morphometry files using \code{\link{write.smp.brainvoyager}}. Modify as needed before writing.
#' @export
bvsmp <- function(morph_data) {
  ret_list = list();
  ret_list$smp_version = 3L;


  ret_list$num_mesh_vertices = length(morph_data);
  ret_list$num_maps = 1L;
  ret_list$srf_file_name = "";
  ret_list$vertex_maps = list();

  vm = list();
  vm$map_type = 13L;
  vm$num_lags = 0L;
  vm$min_lag = 0L;
  vm$max_lag = 0L;
  vm$cc_overlay = 0L;
  vm$cluster_size = 0L;
  vm$enable_cluster_check = 0L;
  vm$stat_threshold_critical = 0.0;
  vm$stat_threshold_max = 0.0;
  vm$degrees_of_freedom_1_fnom = 0L;
  vm$degrees_of_freedom_2_fdenom = 0L;
  vm$cortex_bonferroni_correct = 0L;
  vm$color_critical_rgb = c(100L, 0L, 0L);
  vm$color_max_rgb = c(100L, 100L, 0L);
  vm$enable_smp_color = 0L;
  vm$transparent_color_factor = 0.0;
  vm$map_name = "data"; # not very creative, I know.
  vm$data = morph_data;
  ret_list$vertex_maps[[1]] = vm;

  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}
