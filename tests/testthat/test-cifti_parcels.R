# Tests for building CIFTI-2 parcel axes from brain surface annotations (R/cifti_parcels.R).
#
# The synthetic annotations below build an fs.annot instance in memory (the class is what
# is.fs.annot() checks), so that the region names, their order in the label table and the
# vertex indices can be controlled exactly. The shipped FreeSurfer annotation
# (inst/extdata/lh.aparc.annot.gz) is used for a realistic check at the end.

cifti.test.annot.from.names <- function(label_names, region_names = NULL) {
  if (is.null(region_names)) {
    region_names <- unique(label_names[nzchar(label_names)])
  }
  codes <- match(label_names, region_names) - 1L
  codes[is.na(codes)] <- length(region_names) + 10L # a code that is not in the label table
  n_regions <- length(region_names)
  annot <- list(
    vertices = seq_along(label_names) - 1L,
    label_codes = as.integer(codes),
    label_names = as.character(label_names),
    colortable = list(
      num_entries = n_regions,
      struct_names = as.character(region_names),
      table = cbind(rep(0., n_regions), rep(0., n_regions), rep(0., n_regions),
                    rep(255., n_regions), as.numeric(seq_len(n_regions) - 1L))
    ),
    colortable_df = data.frame(struct_name = region_names, stringsAsFactors = FALSE),
    metadata = list()
  )
  class(annot) <- c("fs.annot", "list")
  return(annot)
}

test_that("Hemisphere markers are removed from region names", {
  expect_equal(cifti.region.name.without.hemisphere(c("L_insula", "R_insula")), c("insula", "insula"))
  expect_equal(cifti.region.name.without.hemisphere(c("LH_insula", "RH_insula")), c("insula", "insula"))
  expect_equal(cifti.region.name.without.hemisphere(c("Left_insula", "Right_insula")), c("insula", "insula"))
  expect_equal(cifti.region.name.without.hemisphere(c("insula_L", "insula_R")), c("insula", "insula"))
  expect_equal(cifti.region.name.without.hemisphere(c("insula-lh", "insula-rh")), c("insula", "insula"))
  expect_equal(cifti.region.name.without.hemisphere(c("7Networks_LH_Vis_1", "7Networks_RH_Vis_1")),
               c("7Networks_Vis_1", "7Networks_Vis_1"))
  # Names without a marker are returned unchanged, and a marker alone is not a marker.
  expect_equal(cifti.region.name.without.hemisphere(c("superiorfrontal", "L", "R", "Leftover_region")),
               c("superiorfrontal", "L", "R", "Leftover_region"))
  expect_error(cifti.region.name.without.hemisphere(1:3), "character vector")
})

test_that("A parcels axis is built from two annotations grouped by region name", {
  lh_annot <- cifti.test.annot.from.names(c("L_insula", "L_insula", "L_unknown"))
  rh_annot <- cifti.test.annot.from.names(c("R_insula", "R_vis", "R_insula"))

  axis <- cifti.axis.parcels.from.annot(list(lh = lh_annot, rh = rh_annot))
  expect_equal(axis$type, "CIFTI_INDEX_TYPE_PARCELS")
  expect_length(axis$parcels, 3L)
  expect_equal(vapply(axis$parcels, function(parcel) parcel$name, character(1L)),
               c("insula", "unknown", "vis"))
  # The vertices of a region that occurs in both hemispheres end up in one parcel, listed
  # per structure, and structures without vertices for a region are left out.
  expect_equal(axis$parcels[[1L]]$vertices, list(CORTEX_LEFT = c(0L, 1L), CORTEX_RIGHT = c(0L, 2L)))
  expect_equal(axis$parcels[[2L]]$vertices, list(CORTEX_LEFT = 2L))
  expect_equal(axis$parcels[[3L]]$vertices, list(CORTEX_RIGHT = 1L))
  expect_equal(vapply(axis$parcels, function(parcel) parcel$index, integer(1L)), 0:2)
  # The surface sizes are derived from the vertex indices of the annotations.
  expect_equal(cifti.axis.surface.sizes(axis), c(CORTEX_LEFT = 3L, CORTEX_RIGHT = 3L))
  expect_equal(cifti.axis.size(axis), 3L)
})

test_that("The parcels of an annotation are ordered like its label table", {
  # The vertices are in an arbitrary order, the label table defines the atlas order.
  annot <- cifti.test.annot.from.names(c("B", "C", "A", "B"), region_names = c("A", "B", "C"))
  axis <- cifti.axis.parcels.from.annot(annot, structure = "lh")
  expect_equal(vapply(axis$parcels, function(parcel) parcel$name, character(1L)), c("A", "B", "C"))
  expect_equal(axis$parcels[[1L]]$vertices$CORTEX_LEFT, 2L)
  expect_equal(axis$parcels[[2L]]$vertices$CORTEX_LEFT, c(0L, 3L))
  # Region names in the label table that have no vertices do not become parcels.
  annot_empty_region <- cifti.test.annot.from.names(c("A", "A"), region_names = c("A", "B"))
  axis_empty <- cifti.axis.parcels.from.annot(annot_empty_region, structure = "lh")
  expect_length(axis_empty$parcels, 1L)
  expect_equal(axis_empty$parcels[[1L]]$name, "A")
})

test_that("Vertices without a region name get the default label name", {
  annot <- cifti.test.annot.from.names(c("A", "", "A", ""))
  axis <- cifti.axis.parcels.from.annot(annot, structure = "lh")
  expect_equal(vapply(axis$parcels, function(parcel) parcel$name, character(1L)), c("A", "unknown"))
  expect_equal(axis$parcels[[2L]]$vertices$CORTEX_LEFT, c(1L, 3L))

  axis_named <- cifti.axis.parcels.from.annot(annot, structure = "lh", default_label_name = "medialwall")
  expect_equal(vapply(axis_named$parcels, function(parcel) parcel$name, character(1L)), c("A", "medialwall"))
  expect_error(cifti.axis.parcels.from.annot(annot, structure = "lh", default_label_name = ""),
               "non-empty character string")
})

test_that("The input forms and the brain structures of the annotations are checked", {
  annot <- cifti.test.annot.from.names(c("A", "A"))
  expect_error(cifti.axis.parcels.from.annot(annot), "has to be given as a single character string")
  expect_error(cifti.axis.parcels.from.annot(list(annot)), "must be a named list")
  expect_error(cifti.axis.parcels.from.annot(list(lh = annot, lh = annot)), "two annotations for the same brain structure")
  expect_error(cifti.axis.parcels.from.annot(list(lh = annot, rh = list(a = 1))), "is not an annotation")
  expect_error(cifti.axis.parcels.from.annot(list(lh = 1)), "is not an annotation")
  expect_error(cifti.axis.parcels.from.annot("no annot"), "must be an fs.annot instance")

  # Structures are accepted in any spelling, and a single annotation needs one.
  axis_lh <- cifti.axis.parcels.from.annot(annot, structure = "CORTEX_LEFT")
  expect_equal(names(axis_lh$parcels[[1L]]$vertices), "CORTEX_LEFT")
  axis_rh <- cifti.axis.parcels.from.annot(list(rh = annot))
  expect_equal(names(axis_rh$parcels[[1L]]$vertices), "CORTEX_RIGHT")

  # An annotation without a label table cannot name its regions.
  annot_no_table <- annot
  annot_no_table$label_names <- NULL
  expect_error(cifti.axis.parcels.from.annot(annot_no_table, structure = "lh"), "has no label names")
})

test_that("Parcels can be selected and ordered by name", {
  annot <- cifti.test.annot.from.names(c("A", "B", "C", "B"))
  axis <- cifti.axis.parcels.from.annot(annot, structure = "lh", parcel_names = c("C", "A"))
  expect_equal(vapply(axis$parcels, function(parcel) parcel$name, character(1L)), c("C", "A"))
  expect_equal(vapply(axis$parcels, function(parcel) parcel$index, integer(1L)), 0:1)
  expect_equal(axis$parcels[[1L]]$vertices$CORTEX_LEFT, 2L)

  expect_error(cifti.axis.parcels.from.annot(annot, structure = "lh", parcel_names = "D"), "no parcel named")
  expect_error(cifti.axis.parcels.from.annot(annot, structure = "lh", parcel_names = ""), "non-empty parcel names")
  expect_error(cifti.axis.parcels.from.annot(annot, structure = "lh", parcel_names = 1:2), "character vector")
})

test_that("The surface size is the size of the surface the vertex indices refer to", {
  # A partial annotation (e.g. one that was derived from a grayordinates file) still
  # reports the size of the complete surface.
  annot <- cifti.test.annot.from.names(c("A", "B"))
  annot$vertices <- c(2L, 9L)
  axis <- cifti.axis.parcels.from.annot(annot, structure = "lh")
  expect_equal(cifti.axis.surface.sizes(axis), c(CORTEX_LEFT = 10L))
  expect_equal(axis$parcels[[1L]]$vertices$CORTEX_LEFT, 2L)
  expect_equal(axis$parcels[[2L]]$vertices$CORTEX_LEFT, 9L)
})

test_that("A parcels axis is built from a real FreeSurfer annotation", {
  annot_file <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats")
  annot <- read.fs.annot(annot_file)
  axis <- cifti.axis.parcels.from.annot(annot, structure = "lh")
  region_names <- annot$label_names
  region_names[!nzchar(region_names)] <- "unknown" # the medial wall has no name in the table
  expect_equal(length(axis$parcels), length(unique(region_names)))
  expect_equal(cifti.axis.size(axis), length(unique(region_names)))
  expected_counts <- as.integer(table(region_names)[vapply(axis$parcels, function(parcel) parcel$name, character(1L))])
  expect_equal(vapply(axis$parcels, function(parcel) length(parcel$vertices$CORTEX_LEFT), integer(1L)),
               expected_counts)
  # Every vertex is in exactly one parcel, and the parcels are in atlas order.
  expect_equal(sum(vapply(axis$parcels, function(parcel) length(parcel$vertices$CORTEX_LEFT), integer(1L))),
               length(annot$vertices))
  expect_equal(axis$parcels[[1L]]$name, "unknown")
  expect_equal(axis$parcels[[2L]]$name, "bankssts")
  expect_equal(cifti.axis.surface.sizes(axis), c(CORTEX_LEFT = 149244L))
  expect_equal(sort(unlist(lapply(axis$parcels, function(parcel) parcel$vertices$CORTEX_LEFT))),
               annot$vertices)

  # The axis can be written to a parcellated file and read back.
  out_file <- file.path(tempdir(), "annot_parcels.pscalar.nii")
  write.fs.parcellated.cifti(out_file, as.numeric(seq_along(axis$parcels)), axes = axis, map_names = "region_size")
  back <- read.cifti(out_file)
  expect_equal(as.character(cifti.parcels(back$header, 1L)$name),
               vapply(axis$parcels, function(parcel) parcel$name, character(1L)))
  expect_equal(as.numeric(back$data[1L, ]), as.numeric(seq_along(axis$parcels)))
  unlink(out_file)
})

test_that("An axis built from two hemispheres spans both structures", {
  annot_file <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats")
  annot <- read.fs.annot(annot_file)
  axis_two <- cifti.axis.parcels.from.annot(list(lh = annot, rh = annot))
  axis_one <- cifti.axis.parcels.from.annot(annot, structure = "lh")
  expect_equal(length(axis_two$parcels), length(axis_one$parcels))
  expect_equal(names(axis_two$parcels[[2L]]$vertices), c("CORTEX_LEFT", "CORTEX_RIGHT"))
  expect_equal(axis_two$parcels[[2L]]$vertices$CORTEX_LEFT, axis_one$parcels[[2L]]$vertices$CORTEX_LEFT)
  expect_equal(cifti.axis.surface.sizes(axis_two), c(CORTEX_LEFT = 149244L, CORTEX_RIGHT = 149244L))
})
