# Write data to a gifti file.

Write data to a gifti file.

## Usage

``` r
gifti_writer(filepath, ...)
```

## Arguments

- filepath:

  path to the output gifti file

- ...:

  parameters passed to
  [`gifti_xml`](https://dfsp-spirit.github.io/freesurferformats/reference/gifti_xml.md).

## References

<https://www.nitrc.org/frs/download.php/2871/GIFTI_Surface_Format.pdf>

## Examples

``` r
if (FALSE) { # \dontrun{
outfile <- tempfile(fileext = ".gii")
dataarrays <- list(rep(3.1, 3L), matrix(seq(6), nrow = 2L))
gifti_writer(outfile, dataarrays, datatype = c("NIFTI_TYPE_FLOAT32", "NIFTI_TYPE_INT32"))
} # }
```
