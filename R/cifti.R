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



#' @export
# cf = "~/data/cifti/Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii";
# sf = "~/data/cifti/Conte69.L.inflated.32k_fs_LR.surf.gii";
read.fs.morph.cifti <- function(filepath, brain_structure='CIFTI_STRUCTURE_CORTEX_LEFT', data_column=1L) {

  if(brain_structure == 'lh') {
    brain_structure='CIFTI_STRUCTURE_CORTEX_LEFT';
  }
  if(brain_structure == 'rh') {
    brain_structure='CIFTI_STRUCTURE_CORTEX_RIGHT';
  }

  cii = cifti::read_cifti(filepath);
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
  if(surface_num_verts != length(morph_data_values)) {
    message(sprintf("Surface has %d vertices and data for %d vertices given, the rest will be NA.\n", surface_num_verts, length(morph_data_values)));
  }
  if(length(morph_data_cii_vert_indices) != length(morph_data_values)) {
    warning(sprintf("There are %d vertex indices given for the %d morphometry data values.\n", length(morph_data_cii_vert_indices), length(morph_data_values)));
  }
  message(sprintf("Extracted %d morph data values, from CIFTI index %d to %d.\n", length(morph_data_values), md_index_offset, end_index));
  morph_data[morph_data_cii_vert_indices] = morph_data_values;
  return(morph_data);
}
