# Remove hemisphere markers from region names of a parcellation.

Region names of brain atlases often carry a marker that identifies the
hemisphere a region belongs to, e.g. `L_superiorfrontal` /
`R_superiorfrontal`, `pericalcarine_LH` / `pericalcarine_RH`, or
`7Networks_LH_Vis_1` / `7Networks_RH_Vis_1`. The same region of the two
hemispheres has to be recognized as one parcel when building a parcel
axis for a CIFTI-2 file, so these markers are removed here.

A marker that is removed is one of `L`, `R`, `LH`, `RH`, `Left` or
`Right` (matched case-insensitively), at the start of the name or at its
end, followed or preceded by `_` or `-`, or enclosed by two separators
anywhere in the name (in which case the two separators collapse into
one). Names without such a marker are returned unchanged, and so are
names that consist of nothing but a marker.

## Usage

``` r
cifti.region.name.without.hemisphere(region_names)
```

## Arguments

- region_names:

  character vector, the region names.

## Value

character vector, the names without hemisphere markers.
