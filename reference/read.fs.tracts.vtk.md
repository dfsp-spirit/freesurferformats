# Read VTK legacy format streamlines as tracts.

Reads streamline (tractography) data from files in the VTK legacy
format, i.e. from POLYDATA datasets that contain a LINES section. Both
the ASCII and the binary encoding are supported, as are the cell array
layouts written by VTK 4.2 and older and by VTK 5.1 and newer. This is
the format that Paraview, TrackVis and DSI Studio export streamlines in.
Attribute data such as scalar values per point is ignored.

## Usage

``` r
read.fs.tracts.vtk(filepath)
```

## Arguments

- filepath:

  character string, path to the input file in VTK legacy format.

## Value

an
[`fs.tracts`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.tracts.md)
instance with the streamlines. Use `tracts[[i]]` to get the n x 3
coordinate matrix of a single streamline, and
[`fs.tracts.lengths`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.tracts.lengths.md)
for the number of points of each streamline.

## Note

Points that are not part of any streamline are dropped, because the
`fs.tracts` data structure stores the coordinates of the streamlines
without gaps. A warning is issued if that happens.

## Examples

``` r
# The example file was written by VTK and contains 3 streamlines.
# It also has a point that is not part of any streamline, which is dropped with a warning.
tracts_file <- system.file("extdata", "tracts_v51_binary.vtk",
  package = "freesurferformats", mustWork = TRUE
)
tracts <- suppressWarnings(read.fs.tracts.vtk(tracts_file))
cat(sprintf(
  "Read %d streamlines with %d points.\n",
  length(tracts), nrow(fs.tracts.coords(tracts))
))
#> Read 3 streamlines with 11 points.
```
