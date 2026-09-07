# Functions for working with CIFTI files.
#
# We use the 'cifti' package by J. Muschelli to access data in CIFTI files whenever possible. It seems in 2020 we only need to care about CIFTI v2.
#
# General CIFTI information: https://www.nitrc.org/projects/cifti/
# Specs:
#  - CIFTI v1 spec: https://www.nitrc.org/plugins/mwiki/index.php/cifti:Cifti-1
#  - CITFI v2 full spec (PDF): https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955
# Related:
#  - CIFTI v2 changes from v1: https://www.nitrc.org/forum/forum.php?thread_id=4381&forum_id=1955
#  - Great article on NIFTI v2 format by A Winkler: https://brainder.org/2015/04/03/the-nifti-2-file-format/


#' @title Read surface morphometry data from CIFTI dscalar files.
#'
#' @description Used the 'cifti' package to load the full data from a CIFTI file, then extracts and reconstructs the data for a surface, based on the metadata like vertex counts, indices and offset in the CIFTI file.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dscalar.nii'. Note that this is NOT a NIFTI file, despite the '.nii' part. It uses a CIFTIv2 header though. See the spec for details.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', the concatenated data for 'lh' (first) and 'rh' will be returned, but you will get no information on hemi boundaries. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @param data_column integer, the data column to return. A CIFTI file can contain several measures in different data columns (e.g., cortical thickness and surface area) in a single file. This specifies which column/measure you want. The columns are not named, so you will need to know this in advance if the file has several measures.
#'
#' @return The reconstructed data for the given surface, one value per vertex in the surface. The value for vertices which did not have a value in the CIFTI data is set to `NA`.
#'
#' @examples
#' \dontrun{
#' # Downloaded CIFTI2 example data from https://www.nitrc.org/projects/cifti/
#' cifti_example_data_dir <- "~/data/cifti"
#' cii_file <- file.path(
#'   cifti_example_data_dir,
#'   "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii"
#' )
#' sf_lh <- freesurferformats::read.fs.surface(file.path(
#'   cifti_example_data_dir,
#'   "Conte69.L.inflated.32k_fs_LR.surf.gii"
#' ))
#' sf_rh <- freesurferformats::read.fs.surface(file.path(
#'   cifti_example_data_dir,
#'   "Conte69.R.inflated.32k_fs_LR.surf.gii"
#' ))
#' morph_lh <- read.fs.morph.cifti(cii_file, "lh")
#' # Myelin data
#' morph_rh <- read.fs.morph.cifti(cii_file, "rh")
#' morph2_lh <- read.fs.morph.cifti(cii_file, "lh", 2)
#' # Cortical Thickness data
#' morph2_rh <- read.fs.morph.cifti(cii_file, "rh", 2L)
#' # fsbrain::vis.fs.surface(sf_lh, per_vertex_data = morph_lh);
#' # fsbrain::vis.fs.surface(sf_rh, per_vertex_data = morph_rh);
#' # fsbrain::vis.fs.surface(list('lh'=sf_lh, 'rh'=sf_rh),
#' # per_vertex_data = list('lh'=morph2_lh, 'rh'=morph2_rh));
#' }
#'
#' @note This function calls code from the 'cifti' package by John Muschelli: \url{https://CRAN.R-project.org/package=cifti}.
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.morph.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT", data_column = 1L) {
  cii <- .get.cifti(filepath)

  if (identical(brain_structure, "both")) {
    return(c(read.fs.morph.cifti(cii, brain_structure = "lh", data_column = data_column),
             read.fs.morph.cifti(cii, brain_structure = "rh", data_column = data_column)))
  }
  brain_structure <- .normalize.cifti.brain.structure(brain_structure)
  return(.cifti.surface.matrix(cii, brain_structure)[, data_column])
}


#' @title Read surface parcellation data from CIFTI dlabel files.
#'
#' @description Uses the 'cifti' package to load the data from a CIFTI dlabel file (a dense surface parcellation, i.e., an integer label key per vertex) and returns the per-vertex label keys for a single brain structure. This is the CIFTI analogue of a FreeSurfer annotation file.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dlabel.nii'. Note that this is NOT a NIfTI file, despite the '.nii' part; it uses a CIFTI 2 header instead. See the spec for details.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', the concatenated data for 'lh' (first) and 'rh' will be returned, but you will get no information on hemi boundaries. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @param data_column integer, the data column (map) to return. A CIFTI dlabel file can contain several parcellations in different columns in a single file; this selects which one. The columns are not named, so you will need to know this in advance if the file has several maps (you can use the 'with_label_table' option to inspect the map names).
#'
#' @param with_label_table logical, whether to also return the label table (the mapping from label key to region name and RGBA color) for the selected map. If 'TRUE', the return value is a list, see the Value section.
#'
#' @return If 'with_label_table' is 'FALSE' (the default), an integer vector with one label key per vertex in the surface. Vertices which did not have a value in the CIFTI data are set to `NA`; a key of 0 denotes the unknown / medial wall label (see the label table). If 'with_label_table' is 'TRUE', a named list with entries: 'label_keys' (the integer vector), 'label_table' (a data.frame with columns 'Key', 'Red', 'Green', 'Blue', 'Alpha' and 'Label', where the RGBA values are in the range 0 to 1), 'map_name' (the name of the selected map as a character string, or 'NULL' if unnamed), and 'brain_structure' (a character string). If 'brain_structure' is 'both' and 'with_label_table' is 'TRUE', a named list with entries 'lh' and 'rh', each as described above.
#'
#' @examples
#' \dontrun{
#' # Downloaded CIFTI2 example data from https://www.nitrc.org/projects/cifti/
#' cifti_example_data_dir <- "~/data/cifti"
#' dlabel_file <- file.path(cifti_example_data_dir,
#'   "Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii");
#' parc_lh <- read.fs.parcellation.cifti(dlabel_file, "lh");
#' parc_rh <- read.fs.parcellation.cifti(dlabel_file, "rh");
#' parc_with_table <- read.fs.parcellation.cifti(dlabel_file, "lh",
#'   with_label_table = TRUE);
#' # fsbrain::vis.fs.surface(sf_lh, per_vertex_data = parc_lh);
#' }
#'
#' @note This function calls code from the 'cifti' package by John Muschelli: \url{https://CRAN.R-project.org/package=cifti}.
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.parcellation.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT", data_column = 1L, with_label_table = FALSE) {
  cii <- .get.cifti(filepath)

  if (identical(brain_structure, "both")) {
    if (isTRUE(with_label_table)) {
      return(list(lh = read.fs.parcellation.cifti(cii, "lh", data_column = data_column, with_label_table = TRUE),
                  rh = read.fs.parcellation.cifti(cii, "rh", data_column = data_column, with_label_table = TRUE)))
    }
    return(c(read.fs.parcellation.cifti(cii, "lh", data_column = data_column, with_label_table = FALSE),
             read.fs.parcellation.cifti(cii, "rh", data_column = data_column, with_label_table = FALSE)))
  }

  brain_structure <- .normalize.cifti.brain.structure(brain_structure)
  label_keys <- as.integer(.cifti.surface.matrix(cii, brain_structure)[, data_column])

  if (!isTRUE(with_label_table)) {
    return(label_keys)
  }
  return(list(label_keys = label_keys,
              label_table = .cifti.label.table(cii, data_column),
              map_name = .cifti.map.name(cii, data_column),
              brain_structure = brain_structure))
}


#' @title Read surface time series data from CIFTI dtseries files.
#'
#' @description Uses the 'cifti' package to load the data from a CIFTI dtseries file (a dense surface time series) and returns the per-vertex time series for a single brain structure as a matrix.
#'
#' @param filepath character string, the full path to a file in CIFTI 2 format, should end with '.dtseries.nii'. Note that this is NOT a NIfTI file, despite the '.nii' part; it uses a CIFTI 2 header instead. See the spec for details.
#'
#' @param brain_structure character string or integer, the brain structure for which the data should be extracted from the file. Can be a CIFTI brain structure string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or 'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which are used as aliases for the former). If you specify 'both', a named list with entries 'lh' and 'rh' will be returned. If it is an integer, it will be interpreted as an index into the list of structures within the CIFTI file, use with care.
#'
#' @return A numeric matrix with one row per vertex in the surface and one column per time point (series point) in the file. The value for vertices which did not have a value in the CIFTI data is set to `NA`. If 'brain_structure' is 'both', a named list with entries 'lh' and 'rh', each a matrix as described above.
#'
#' @examples
#' \dontrun{
#' # Downloaded CIFTI2 example data from https://www.nitrc.org/projects/cifti/
#' cifti_example_data_dir <- "~/data/cifti"
#' dtseries_file <- file.path(cifti_example_data_dir,
#'   "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii");
#' series_lh <- read.fs.series.cifti(dtseries_file, "lh");
#' series_both <- read.fs.series.cifti(dtseries_file, "both");
#' }
#'
#' @note This function calls code from the 'cifti' package by John Muschelli: \url{https://CRAN.R-project.org/package=cifti}.
#'
#' @references See \url{https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955} for the CIFTI 2 file format spec. See \url{https://www.nitrc.org/projects/cifti/} for more details on CIFTI, including example files.
#' @export
read.fs.series.cifti <- function(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT") {
  cii <- .get.cifti(filepath)

  if (identical(brain_structure, "both")) {
    return(list(lh = read.fs.series.cifti(cii, "lh"),
                rh = read.fs.series.cifti(cii, "rh")))
  }
  brain_structure <- .normalize.cifti.brain.structure(brain_structure)
  return(.cifti.surface.matrix(cii, brain_structure))
}


# --- Internal helpers (not exported) -----------------------------------------

# Return a parsed CIFTI object, reading the file if needed, and require the 'cifti' package.
.get.cifti <- function(filepath) {
  if (!requireNamespace("cifti", quietly = TRUE)) {
    stop("Reading files in CIFTI format requires the 'cifti' package to be installed.") # nocov
  }
  if (cifti::is.cifti(filepath)) {
    return(filepath)
  }
  return(cifti::read_cifti(filepath))
}

# Map the user-facing 'lh' / 'rh' aliases to canonical CIFTI brain structure names.
.normalize.cifti.brain.structure <- function(brain_structure) {
  if (is.character(brain_structure)) {
    if (identical(brain_structure, "lh")) {
      return("CIFTI_STRUCTURE_CORTEX_LEFT")
    }
    if (identical(brain_structure, "rh")) {
      return("CIFTI_STRUCTURE_CORTEX_RIGHT")
    }
  }
  return(brain_structure)
}

# Resolve a brain structure specifier (name or index) to an index into cii$BrainModel.
.cifti.brain.structure.index <- function(cii, brain_structure) {
  if (is.numeric(brain_structure)) {
    return(as.integer(brain_structure))
  }
  brain_struct_names <- cifti::cifti_brain_structs(cii)
  if (!brain_structure %in% brain_struct_names) {
    stop(sprintf("No brain structure named '%s' in the %d CIFTI brain structures '%s'.\n",
                 brain_structure, length(brain_struct_names),
                 paste(brain_struct_names, collapse = ", ")))
  }
  return(which(brain_struct_names == brain_structure))
}

# Extract the full per-vertex data matrix for one surface brain structure.
# Returns a (SurfaceNumberOfVertices x ncol(cii$data)) numeric matrix, with NA for
# vertices that have no value in the file. Errors for non-surface brain structures.
.cifti.surface.matrix <- function(cii, brain_structure) {
  brain_struct_index <- .cifti.brain.structure.index(cii, brain_structure)
  brain_struct_type <- attr(cii$BrainModel[[brain_struct_index]], "ModelType") # should be "CIFTI_MODEL_TYPE_SURFACE"

  if (is.null(brain_struct_type)) {
    # It's a volume, and the model is just an nx3 integer matrix of n (i,j,k) voxel indices.
    # We still have to confirm this in the spec though, currently this is based on the (official) CIFTI2 example files only.
    stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type NULL (most likely a volume part).\n", as.character(brain_structure)))
  }
  if (brain_struct_type != "CIFTI_MODEL_TYPE_SURFACE") {
    stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type '%s'.\n", as.character(brain_structure), brain_struct_type))
  }

  md_index_count <- attr(cii$BrainModel[[brain_struct_index]], "IndexCount")
  md_index_offset <- attr(cii$BrainModel[[brain_struct_index]], "IndexOffset") + 1L
  vert_indices <- cii$BrainModel[[brain_struct_index]] + 1L
  surface_num_verts <- attr(cii$BrainModel[[brain_struct_index]], "SurfaceNumberOfVertices")

  if (length(vert_indices) != md_index_count) {
    warning(sprintf("There are %d vertex indices given for the %d data values.\n",
                    length(vert_indices), md_index_count))
  }

  data_matrix <- as.matrix(cii$data)
  end_index <- md_index_offset + md_index_count - 1L
  result <- matrix(NA_real_, nrow = surface_num_verts, ncol = ncol(data_matrix))
  result[vert_indices, ] <- data_matrix[md_index_offset:end_index, , drop = FALSE]
  return(result)
}

# Return the label table (key -> name + RGBA) for one map of a CIFTI file.
.cifti.label.table <- function(cii, data_column = 1L) {
  lut <- cii$NamedMap$look_up_table
  if (is.null(lut)) {
    stop("No label table (NamedMap) found in this CIFTI file.")
  }
  n_lut <- length(lut)
  if (data_column < 1L || data_column > n_lut) {
    stop(sprintf("data_column %d is out of range: the file has %d maps (label tables).\n",
                 as.integer(data_column), n_lut))
  }
  return(lut[[data_column]])
}

# Return the name of one map of a CIFTI file, or NULL if it is unnamed or missing.
.cifti.map.name <- function(cii, data_column = 1L) {
  map_names <- cii$NamedMap$map_names
  if (!is.character(map_names) || length(map_names) < data_column) {
    return(NULL)
  }
  if (nzchar(map_names[data_column])) {
    return(map_names[data_column])
  }
  return(NULL)
}
