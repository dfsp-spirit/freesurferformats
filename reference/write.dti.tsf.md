# Write per-point track values to a file in MRtrix TSF format.

The TSF format stores one scalar value per point of a streamline, e.g.,
the fractional anisotropy, the distance along the track or a value
sampled from an image at the point coordinates. It is the companion
format of the TCK format: a TSF file contains no coordinates and no
track boundaries, it is just a stream of values that has to be read
together with the tractogram it describes. The number of values per
track is therefore required to write the file, and a TSF file without
the matching TCK file is meaningless to every reader (MRtrix checks
this, see the note below).

## Usage

``` r
write.dti.tsf(
  tracts,
  filepath,
  lengths = NULL,
  datatype = "Float32LE",
  gzip = NULL,
  header = list()
)
```

## Arguments

- tracts:

  the values to write. This can be an `fs.tracts` instance whose
  `scalars` entry holds a single column of values (as returned by
  [`read.dti.trk`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.trk.md)
  for a file with one scalar, see
  [`fs.tracts`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.tracts.md)
  to construct one), the `scalars` entry of the result of
  [`read.dti.tsf`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tsf.md)
  (so that a scalar file can be read and written back), a list of
  numeric vectors (one per track), or a single numeric vector of all
  values concatenated (in which case `lengths` is required).

- filepath:

  character string, the path of the file to write.

- lengths:

  integer vector or NULL, the number of values per track. This is
  ignored unless `tracts` is a plain vector, and has to be given in that
  case.

- datatype:

  character string, one of 'Float32LE' (the default, and what MRtrix
  writes), 'Float32BE', 'Float64LE' or 'Float64BE'.

- gzip:

  logical or NULL, whether to gzip-compress the output. If `NULL` (the
  default), the file is compressed when the file name ends in '.gz'.

- header:

  named list of additional header entries to store in the file, e.g.,
  the header of the file the tracks were read from. The entries 'id',
  'datatype', 'count', 'file' and 'derived' are always computed by the
  readers and cannot be set.

## Value

the file path, invisibly.

## Note

The TSF format stores a NaN value after every track, and unlike the TCK
format it has no Inf terminator: the reader relies on the delimiters to
split the value stream into tracks. A file whose values are not
delimited exactly like the tracks of the tractogram can therefore not be
detected as broken by this package, but MRtrix reports the mismatch of
the track counts when the file is used (e.g., in `tcksample` or
`tsfvalidate`).

## See also

[`read.dti.tsf`](https://dfsp-spirit.github.io/freesurferformats/reference/read.dti.tsf.md),
[`write.dti.tck`](https://dfsp-spirit.github.io/freesurferformats/reference/write.dti.tck.md)

## Examples

``` r
# A TSF file stores one value per point. Since the format contains no track
# boundaries, the track lengths have to be provided:
tsff <- tempfile(fileext = ".tsf");
values_by_track <- list(c(0.1, 0.2, 0.3), c(0.4, 0.5));
write.dti.tsf(values_by_track, tsff);
read.dti.tsf(tsff)$scalars$scalar_list;
#> [[1]]
#> [1] 0.1 0.2 0.3
#> 
#> [[2]]
#> [1] 0.4 0.5
#> 

# The same file can be written from one vector of values and the lengths:
write.dti.tsf(c(0.1, 0.2, 0.3, 0.4, 0.5), tsff, lengths = c(3L, 2L));

if (FALSE) { # \dontrun{
# Read the values of a track scalar file, modify them and write them back:
tsf <- read.dti.tsf("brain.tsf");
tsf$scalars$merged <- tsf$scalars$merged * 2;
write.dti.tsf(tsf$scalars, "brain_doubled.tsf");

# Sample an image along the tracks of a tractogram and store the result. The
# values of a TRK file that has one scalar are accepted as they are:
trk <- read.dti.trk("brain.trk");
write.dti.tsf(trk$tracks, "brain.trk.tsf");
} # }
```
