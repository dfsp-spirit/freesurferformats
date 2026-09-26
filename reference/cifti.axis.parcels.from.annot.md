# Create a CIFTI-2 parcels axis from brain surface annotations.

A parcellated CIFTI-2 file stores no label table, it describes each
parcel by its name and by the vertices (per brain structure) and volume
voxels it contains. This function builds such a parcels axis from brain
surface annotations (see
[`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md)),
which assign a region label to every vertex of a hemisphere, so that
data that was computed per region of an atlas (a FreeSurfer
parcellation, a Schaefer atlas, ...) can be written to a parcellated
file.

The vertices of a parcel are collected by the *name* of the region, not
by its label key: the hemispheres of an atlas usually use the same names
for the same region, but the names often carry a hemisphere marker (e.g.
`L_superiorfrontal` in one hemisphere and `R_superiorfrontal` in the
other, or `7Networks_LH_Vis_1` and `7Networks_RH_Vis_1`), and a parcel
of a parcellated CIFTI-2 file is a region that spans the structures it
occurs in. The markers are removed before the names are compared, see
[`cifti.region.name.without.hemisphere`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.region.name.without.hemisphere.md)
for the exact rules. Vertices whose label is not in the label table of
the annotation (the medial wall, which is usually the 'unknown' region
with key 0) form a parcel like any other, named `default_label_name`: a
parcellated file has no 'no value' case, and dropping the medial wall
silently would change the data.

## Usage

``` r
cifti.axis.parcels.from.annot(
  annots,
  structure = NULL,
  parcel_names = NULL,
  default_label_name = "unknown"
)
```

## Arguments

- annots:

  an `fs.annot` instance (see
  [`read.fs.annot`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.md)),
  or a named list of them, with the brain structures as the names (e.g.
  `list(lh = lh_annot, rh = rh_annot)`).

- structure:

  character string or `NULL`, the brain structure of the annotation,
  needed if `annots` is a single `fs.annot` instance instead of a named
  list.

- parcel_names:

  character vector or `NULL`, the names of the parcels to write, in that
  order. This selects and orders the parcels of the axis; without it all
  regions of the parcellation are used, in the order in which the label
  table of the annotation lists them (the atlas order, not the order in
  which the vertices happen to be stored in the mesh). Renaming is not
  supported, since the name is what the hemispheres of a region are
  matched by.

- default_label_name:

  character string, the name to use for vertices whose label is not in
  the label table of the annotation.

## Value

an axis of type 'CIFTI_INDEX_TYPE_PARCELS', see
[`cifti.axis.parcels`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.md),
to be passed to
[`write.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md)
or to one of the writers that accept it (e.g.
[`write.fs.parcellated.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md)).

## See also

Other cifti functions:
[`cifti.axis.brain.models()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.brain.models.md),
[`cifti.axis.from.template()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.from.template.md),
[`cifti.axis.labels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.labels.md),
[`cifti.axis.parcels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.md),
[`cifti.axis.scalars()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.scalars.md),
[`cifti.axis.series()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.series.md),
[`cifti.brain.model.surface()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.surface.md),
[`cifti.brain.model.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.brain.model.volume.md),
[`cifti.dim.labels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.labels.md),
[`cifti.file.type.for.axes()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.file.type.for.axes.md),
[`cifti.grayordinates()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md),
[`cifti.header.from.axes()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md),
[`cifti.label.table()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md),
[`cifti.parcel()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcel.md),
[`cifti.parcels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcels.md),
[`cifti.series.info()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.series.info.md),
[`cifti.structure.data()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md),
[`cifti.structures()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structures.md),
[`cifti.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.volume.md),
[`print.fs.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.cifti.md),
[`print.fs.cifti.data()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.cifti.data.md),
[`print.fs.connectome()`](https://dfsp-spirit.github.io/freesurferformats/reference/print.fs.connectome.md),
[`read.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md),
[`read.cifti.header()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md),
[`read.cifti.rows()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md),
[`read.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md),
[`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md),
[`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md),
[`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
[`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md),
[`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md),
[`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)

## Examples

``` r
lh_annot_file <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats")
lh_annot <- read.fs.annot(lh_annot_file)
axis <- cifti.axis.parcels.from.annot(lh_annot, structure = "lh")
length(axis$parcels)
#> [1] 35
axis$parcels[[1L]]$name
#> [1] "unknown"
```
