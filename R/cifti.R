# Functions for working with CIFTI files.
#
# We use the 'cifti' package by J. Muschelli to access data in CIFTI files whenever possible.
# Afaict, that package supports CIFTI v1 only though, so we may need to come up with something for CIFTI v2 files, which
# are not backwards compatible with v1. CIFTI v2 uses a NIFTI v2 header, and it seems 'oro.nifti' only supports readings v1 NIFTI headers,
# so we may need to come up with something there as well, but the changes from the NIFTI v1 header seem minor, mostly datatypes were
# changed to support better precision/large data ranges.
#
# General CIFTI information: https://www.nitrc.org/projects/cifti/
# Specs:d
#  - CIFTI v1 spec: https://www.nitrc.org/plugins/mwiki/index.php/cifti:Cifti-1
#  - CITFI v2 full spec (PDF): https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955
# Related:
#  - CIFTI v2 changes from v1: https://www.nitrc.org/forum/forum.php?thread_id=4381&forum_id=1955
#  - Great article on NIFTI v2 format by A Winkler: https://brainder.org/2015/04/03/the-nifti-2-file-format/


#' @title Read surface morphometry data from CIFTI dscalar files.
#'
#' @description Used the 'cifti' package to load the full data from a CIFTI file, then extracts and reconstructs the data for a surface, folowing the meta data like vertex counts, indices and offset in the CIFTI file.
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
#' cifti_example_data_dir = "~/data/cifti";
#' cii_file = file.path(cifti_example_data_dir,
#'  "Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii");
#' sf_lh = freesurferformats::read.fs.surface(file.path(cifti_example_data_dir,
#'  "Conte69.L.inflated.32k_fs_LR.surf.gii"));
#' sf_rh = freesurferformats::read.fs.surface(file.path(cifti_example_data_dir,
#' "Conte69.R.inflated.32k_fs_LR.surf.gii"));
#' morph_lh = read.fs.morph.cifti(cii_file, 'lh'); # Myelin data
#' morph_rh = read.fs.morph.cifti(cii_file, 'rh');
#' morph2_lh = read.fs.morph.cifti(cii_file, 'lh', 2); # Cortical Thickness data
#' morph2_rh = read.fs.morph.cifti(cii_file, 'rh', 2L);
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
read.fs.morph.cifti <- function(filepath, brain_structure='CIFTI_STRUCTURE_CORTEX_LEFT', data_column=1L) {

  if (requireNamespace("cifti", quietly = TRUE)) {


    if(cifti::is.cifti(filepath)) {
      cii = filepath;
    } else {
      cii = cifti::read_cifti(filepath);
    }

    if(brain_structure == 'lh') {
      brain_structure='CIFTI_STRUCTURE_CORTEX_LEFT';
    } else if (brain_structure == 'rh') {
      brain_structure='CIFTI_STRUCTURE_CORTEX_RIGHT';
    } else if(brain_structure == 'both') {
      return(c(read.fs.morph.cifti(cii, brain_structure = 'lh', data_column = data_column), read.fs.morph.cifti(cii, brain_structure = 'rh', data_column = data_column)));
    }

    if(is.numeric(brain_structure)) {
      brain_struct_index = as.integer(brain_structure);
    } else {
      brain_struct_names = cifti::cifti_brain_structs(cii);
      if(! brain_structure %in% brain_struct_names) {
        stop(sprintf("No brain structure named '%s' in the %d CIFTI brain structures '%s'.\n", brain_structure, length(brain_struct_names), paste(brain_struct_names, collapse = ", ")));
      }
      brain_struct_index = which(brain_struct_names == brain_structure);
    }
    morph_data_cii = as.matrix(cii$data); # The morphometry data, but this is not one value per vertex in the brain model/surface:
    # it is only for the vertices listed below. The other vertices will get NA.
    # Another thing to keep in mind is that a CIFTI file may contain several descriptors per vertex (e.g., thickness and area in a single file), so this is a matrix instead of a vector.
    brain_struct_type = attr(cii$BrainModel[[brain_struct_index]], "ModelType"); # should be "CIFTI_MODEL_TYPE_SURFACE"

    if(is.null(brain_struct_type)) {
      # It's a volume, and the model is just an nx3 integer matrix of n (i,j,k) voxel indices.
      # We still have to confirm this in the spec though, currently this is based on the (official) CIFTI2 example files only.
      stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type NULL (most likely a volume part).\n", as.character(brain_structure)));
    } else {
      if(brain_struct_type != "CIFTI_MODEL_TYPE_SURFACE") {
        stop(sprintf("Currently only model type 'CIFTI_MODEL_TYPE_SURFACE' is supported, but structure '%s' has type '%s'.\n", as.character(brain_structure), brain_struct_type));
      }
    }
    md_index_count = attr(cii$BrainModel[[brain_struct_index]], "IndexCount");
    md_index_offset = attr(cii$BrainModel[[brain_struct_index]], "IndexOffset") + 1L;
    morph_data_cii_vert_indices = cii$BrainModel[[brain_struct_index]] + 1L;
    surface_num_verts = attr(cii$BrainModel[[brain_struct_index]], "SurfaceNumberOfVertices");
    morph_data = rep(NA, surface_num_verts);


    end_index = (md_index_offset+md_index_count-1L);
    morph_data_values = morph_data_cii[md_index_offset:end_index, data_column];
    #if(surface_num_verts != length(morph_data_values)) {
    #  message(sprintf("Surface has %d vertices and data for %d vertices given, the rest will be NA.\n", surface_num_verts, length(morph_data_values)));
    #}
    if(length(morph_data_cii_vert_indices) != length(morph_data_values)) {
      warning(sprintf("There are %d vertex indices given for the %d morphometry data values.\n", length(morph_data_cii_vert_indices), length(morph_data_values)));
    }
    #message(sprintf("Extracted %d morph data values, from CIFTI index %d to %d.\n", length(morph_data_values), md_index_offset, end_index));
    morph_data[morph_data_cii_vert_indices] = morph_data_values;
    return(morph_data);
  } else {
    stop("Reading files in CIFTI format requires the 'cifti' package to be installed.");
  }
}
