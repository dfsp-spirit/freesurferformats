
#' @title Read Brainvoyager statistical surface results from v4 or v5 SMP file.
#'
#' @inheritParams read.smp.brainvoyager
#'
#' @note Do not call this, call \code{read.smp.brainvoyager} instead, which will figure out the version and call the appropriate function.
#'
#' @return named list of file contents
#'
#' @keywords internal
read.smp.brainvoyager.v4or5 <- function(filepath, version) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);

  if(! (ret_list$smp_version %in% c(4L, 5L))) {
    stop(sprintf("Found SMP file in file format version %d, only version 4 or 5 is supported by this function.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  if(ret_list$num_maps > 0L) {
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
      vm$includes_val_greater_threshold_max = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      vm$degrees_of_freedom_1_fnom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      vm$degrees_of_freedom_2_fdenom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      if(ret_list$smp_version == 5L) {
        vm$pos_neg_flag = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      }
      vm$cortex_bonferroni_correct = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      vm$color_min_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
      vm$color_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
      vm$color_negative_min_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
      vm$color_negative_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);

      vm$enable_smp_color = readBin(fh, integer(), size = 1, n = 1, endian = endian);
      if(ret_list$smp_version == 5L) {
        vm$map_lut_name = readBin(fh, character(), n = 1);
      }

      vm$transparent_color_factor = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
      vm$map_name = readBin(fh, character(), n = 1);
      ret_list$vertex_maps[[map_index]] = vm;
    }

    # header done, now read the data
    for(map_index in seq.int(ret_list$num_maps)) {
      ret_list$vertex_maps[[map_index]]$data = readBin(fh, numeric(), size = 4, n = ret_list$num_mesh_vertices, endian = endian);
    }
  }
  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}

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
    stop(sprintf("Found SMP file in file format version %d, only version 3 is supported by this function.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  if(ret_list$num_maps > 0L) {
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
  }
  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}


#' @title Read Brainvoyager statistical surface results from v1 or v2 SMP file.
#'
#' @inheritParams read.smp.brainvoyager
#'
#' @note Do not call this, call \code{read.smp.brainvoyager} instead, which will figure out the version and call the appropriate function.
#'
#' @return named list of file contents
#'
#' @keywords internal
read.smp.brainvoyager.v1or2 <- function(filepath, version) {
  endian = "little";
  fh = file(filepath, "rb");
  on.exit({ close(fh) }, add=TRUE);

  ret_list = list();
  ret_list$smp_version = readBin(fh, integer(), size = 2, n = 1, endian = endian);

  if(!(ret_list$smp_version %in% c(1L, 2L))) {
    stop(sprintf("Found SMP file in file format version %d, only version 1 or 2 is supported by this function.\n", ret_list$smp_version));
  }

  ret_list$num_mesh_vertices = readBin(fh, integer(), size = 4, n = 1, endian = endian);
  ret_list$num_maps = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$map_type = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$num_lags = readBin(fh, integer(), size = 2, n = 1, endian = endian);
  ret_list$srf_file_name = readBin(fh, character(), n = 1);
  ret_list$vertex_maps = list();
  if(ret_list$num_maps > 0L) {
    for(map_index in seq.int(ret_list$num_maps)) {
      vm = list();
      vm$cluster_size = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      if(ret_list$smp_version == 2L) {
        vm$enable_cluster_check = readBin(fh, integer(), size = 1, n = 1, endian = endian);
      } else {
        vm$enable_cluster_check = 1L;
      }
      vm$stat_threshold_critical = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
      vm$stat_threshold_max = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
      vm$degrees_of_freedom_1_fnom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      vm$degrees_of_freedom_2_fdenom = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      vm$cortex_bonferroni_correct = readBin(fh, integer(), size = 4, n = 1, endian = endian);
      if(ret_list$smp_version == 2L) {
        vm$color_critical_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);  # called min rgb in latest documentation
        vm$color_max_rgb = readBin(fh, integer(), size = 1, n = 3, endian = endian);
      }
      vm$enable_smp_color = readBin(fh, integer(), size = 1, n = 1, endian = endian);
      if(ret_list$smp_version == 2L) {
        vm$transparent_color_factor = readBin(fh, numeric(), size = 4, n = 1, endian = endian);
      } else {
        vm$transparent_color_factor = 1.0;
      }
      vm$map_name = readBin(fh, character(), n = 1);
      ret_list$vertex_maps[[map_index]] = vm;
    }

    # header done, now read the data
    for(map_index in seq.int(ret_list$num_maps)) {
      ret_list$vertex_maps[[map_index]]$data = readBin(fh, numeric(), size = 4, n = ret_list$num_mesh_vertices, endian = endian);
    }
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
#' @note Currently only SMP file versions 1 to 5 are supported, as these are the only ones for which a spec is available. The version is encoded in the file header.
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

  if(smp_version == 5L) {
    return(invisible(read.smp.brainvoyager.v4or5(filepath, version = 5L)));
  } else if(smp_version == 4L) {
    return(invisible(read.smp.brainvoyager.v4or5(filepath, version = 4L)));
  } else if(smp_version == 3L) {
    return(invisible(read.smp.brainvoyager.v3(filepath)));
  } else if(smp_version == 2L) {
    return(invisible(read.smp.brainvoyager.v1or2(filepath, version = 2L)));
  } else if(smp_version == 1L) {
    return(invisible(read.smp.brainvoyager.v1or2(filepath, version = 1L)));
  } else {
    stop(sprintf("Found SMP file in file format version %d, only versions 1 to 5 are supported.\n", smp_version));
  }
}


#' @title Check whether object is a bvsmp instance.
#'
#' @param x any `R` object
#'
#' @return TRUE if its argument is an bvsmp instane (that is, has "bvsmp" amongst its classes) and FALSE otherwise.
#' @export
is.bvsmp <- function(x) inherits(x, "bvsmp")


#' @title Write a brainvoyager v3, v4 or v5 SMP file.
#'
#' @inheritParams write.smp.brainvoyager
#'
#' @note Called by \code{\link{write.smp.brainvoyager}}.
#'
#' @keywords internal
write.smp.brainvoyager.v3or4or5 <- function(filepath, bvsmp, smp_version) {
  endian = "little";
  fh = file(filepath, "wb", blocking = TRUE);

  if(!(smp_version %in% c(3L, 4L, 5L))) {
    stop(sprintf("Found SMP file in file format version %d, only versions 3, 4 and 5 are supported by this function.\n", smp_version));
  }

  writeBin(as.integer(smp_version), fh, size = 2, endian = endian);
  writeBin(as.integer(bvsmp$num_mesh_vertices), fh, size = 4, endian = endian);
  writeBin(as.integer(bvsmp$num_maps), fh, size = 2, endian = endian);
  writeChar(bvsmp$srf_file_name, fh);
  if(bvsmp$num_maps > 0L) {
    for(vm in bvsmp$vertex_maps) {

      # set defaults
      if(is.null(vm$enable_cluster_check)) {
        vm$enable_cluster_check = 1L;
      }

      writeBin(as.integer(vm$map_type), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$num_lags), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$min_lag), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$max_lag), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$cc_overlay), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$cluster_size), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$enable_cluster_check), fh, size = 1, endian = endian);
      writeBin(as.double(vm$stat_threshold_critical), fh, size = 4, endian = endian);
      writeBin(as.double(vm$stat_threshold_max), fh, size = 4, endian = endian);

      if(smp_version >= 4L) {
        writeBin(as.integer(vm$includes_val_greater_threshold_max), fh, size = 4, endian = endian);
      }

      writeBin(as.integer(vm$degrees_of_freedom_1_fnom), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$degrees_of_freedom_2_fdenom), fh, size = 4, endian = endian);

      if(smp_version >= 5L) {
        writeBin(as.integer(vm$pos_neg_flag), fh, size = 4, endian = endian);
      }

      writeBin(as.integer(vm$cortex_bonferroni_correct), fh, size = 4, endian = endian);
      writeBin(as.integer(vm$color_critical_rgb), fh, size = 1, endian = endian); # color_critical_rgb is vector of length 3
      writeBin(as.integer(vm$color_max_rgb), fh, size = 1, endian = endian); # color_max_rgb is vector of length 3

      if(smp_version >= 4L) {
        writeBin(as.integer(vm$color_negative_min_rgb), fh, size = 1, endian = endian); # a vector of length 3
        writeBin(as.integer(vm$color_negative_max_rgb), fh, size = 1, endian = endian); # a vector of length 3
      }

      writeBin(as.integer(vm$enable_smp_color), fh, size = 1, endian = endian);

      if(smp_version >= 5L) {
        writeChar(vm$map_lut_name, fh);
      }

      writeBin(as.double(vm$transparent_color_factor), fh, size = 4, endian = endian);
      writeChar(vm$map_name, fh);
    }

    for(vm in bvsmp$vertex_maps) { # write data
      writeBin(as.double(vm$data), fh, size = 4, endian = endian);
    }
  }
  close(fh);
}


#' @title Write a brainvoyager v2 SMP file.
#'
#' @inheritParams write.smp.brainvoyager
#'
#' @note Called by \code{\link{write.smp.brainvoyager}}.
#'
#' @note The map_type and num_lags of the first vertex map will be used for the top header,i.e., for all maps. The v2 format does not support per-map settings for these values. Also min_alg, max_lag and cc_overlay are ignored.
#'
#' @keywords internal
write.smp.brainvoyager.v2 <- function(filepath, bvsmp) {
  endian = "little";
  fh = file(filepath, "wb", blocking = TRUE);

  smp_version = 2L;

  writeBin(as.integer(smp_version), fh, size = 2, endian = endian);
  writeBin(as.integer(bvsmp$num_mesh_vertices), fh, size = 4, endian = endian);
  writeBin(as.integer(bvsmp$num_maps), fh, size = 2, endian = endian);
  writeBin(as.integer(bvsmp$vertex_maps[[1]]$map_type), fh, size = 2, endian = endian);
  writeBin(as.integer(bvsmp$vertex_maps[[1]]$num_lags), fh, size = 2, endian = endian);
  writeChar(bvsmp$srf_file_name, fh);
  if(bvsmp$num_maps > 0L) {
    for(vm in bvsmp$vertex_maps) {
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
      writeChar(vm$map_name, fh);
    }

    for(vm in bvsmp$vertex_maps) { # write data
      writeBin(as.double(vm$data), fh, size = 4, endian = endian);
    }
  }
  close(fh);
}



#' @title Write a brainvoyager SMP file.
#'
#' @description Write a brainvoyager SMP file, which contains one or more vertex-wise data maps (stats or morphometry data).
#'
#' @param filepath character string, the output file
#'
#' @param bvsmp bvsmp instance, a named list as returned by \code{\link{read.smp.brainvoyager}}.
#'
#' @param smp_version integer, the SMP file format version to use when writing. Versions 2 to 5 are supported, but only versions 2 and 3 have been tested properly. Please report any problems you encounter. When converting between file versions (e.g., loading a v2 file and saving the result as a v5 file), some required fields may be missing, and for those without a default value according to the official spec, you will have to manually add the value you want in the bvsmp object before writing.
#'
#' @seealso \code{\link{write.fs.morph.smp}}
#'
#' @export
write.smp.brainvoyager <- function(filepath, bvsmp, smp_version = 3L) {
  if(! is.bvsmp(bvsmp)) {
    stop("Parameter 'bvsmp' must contain bvsmp instance.\n");
  }
  if(smp_version %in% c(3L, 4L, 5L)) {
    return(write.smp.brainvoyager.v3or4or5(filepath, bvsmp, smp_version));
  } else if(smp_version == 2L) {
    return(write.smp.brainvoyager.v2(filepath, bvsmp));
  } else {
    stop(sprintf("Brainvoyager SMP file format version not supported, only versions 2 to 5 are supported.\n"));
  }
}



#' @title Create new bvsmp instance encoding morph data for Brainvoyager.
#'
#' @param morph_data numeric vector, the morphometry data to store in the bvsmp instance (one value per mesh vertex).
#'
#' @return bvsmp instance, can be used to write Brainvoyager SMP format morphometry files using \code{\link{write.smp.brainvoyager}}. Modify as needed before writing.
#'
#' @examples
#'    morph_data = rnorm(100L, 3.0, 1.0);
#'    mybvsmp = bvsmp(morph_data);
#'    mybvsmp$smp_version;
#'
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
  vm$cluster_size = 300L;
  vm$enable_cluster_check = 0L;
  vm$stat_threshold_critical = 0.1;
  vm$stat_threshold_max = 1.0;
  vm$degrees_of_freedom_1_fnom = 200L;
  vm$degrees_of_freedom_2_fdenom = 80L;
  vm$cortex_bonferroni_correct = 2L;
  vm$color_critical_rgb = c(100L, 50L, 50L);
  vm$color_max_rgb = c(110L, 10L, 10L);
  vm$enable_smp_color = 0L;
  vm$transparent_color_factor = 0.0;
  vm$map_name = "data"; # not very creative, I know.
  vm$data = morph_data;
  ret_list$vertex_maps[[1]] = vm;

  class(ret_list) = c("bvsmp", class(ret_list));
  return(ret_list);
}
