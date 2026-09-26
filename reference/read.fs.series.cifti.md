# Read surface time series data from CIFTI dtseries files.

Read the data from a CIFTI dtseries file (a dense surface time series)
and return the per-vertex time series for a single brain structure as a
matrix.

## Usage

``` r
read.fs.series.cifti(filepath, brain_structure = "CIFTI_STRUCTURE_CORTEX_LEFT")
```

## Arguments

- filepath:

  character string, the full path to a file in CIFTI 2 format, should
  end with '.dtseries.nii'. Note that this is NOT a NIfTI file, despite
  the '.nii' part; it uses a CIFTI 2 header instead. See the spec for
  details. An 'fs.cifti' object from
  [`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md)
  or an 'fs.cifti.data' object from
  [`read.cifti`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md)
  may be given instead of a path (this is faster if you need the data of
  several structures). An object created by the 'cifti' package is still
  accepted for backwards compatibility.

- brain_structure:

  character string or integer, the brain structure for which the data
  should be extracted from the file. Can be a CIFTI brain structure
  string (one of 'CIFTI_STRUCTURE_CORTEX_LEFT' or
  'CIFTI_STRUCTURE_CORTEX_RIGHT'), or simply one of 'lh', 'rh' (which
  are used as aliases for the former). If you specify 'both', a named
  list with entries 'lh' and 'rh' will be returned. If it is an integer,
  it will be interpreted as an index into the list of structures within
  the CIFTI file, use with care.

## Value

A numeric matrix with one row per vertex in the surface and one column
per time point (series point) in the file. The value for vertices which
did not have a value in the CIFTI data is set to `NA`. If
'brain_structure' is 'both', a named list with entries 'lh' and 'rh',
each a matrix as described above.

## References

See
<https://www.nitrc.org/forum/attachment.php?attachid=341&group_id=454&forum_id=1955>
for the CIFTI 2 file format spec. See
<https://www.nitrc.org/projects/cifti/> for more details on CIFTI,
including example files.

## Examples

``` r
cifti_file <- system.file("extdata", "cifti", "tiny.dtseries.nii", package = "freesurferformats")
series_lh <- read.fs.series.cifti(cifti_file, "lh")
dim(series_lh)
#> [1] 10  4
series_both <- read.fs.series.cifti(cifti_file, "both")
names(series_both)
#> [1] "lh" "rh"
```
