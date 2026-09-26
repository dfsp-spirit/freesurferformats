# Extract the header information and the data of an `oro.nifti` instance into a plain list.

Extract the header information and the data of an `oro.nifti` instance
into a plain list.

## Usage

``` r
nifti.info.from.oro.instance(nifti_img)
```

## Arguments

- nifti_img:

  an instance of class `nifti` from the `oro.nifti` package.

## Value

named list, see
[`nifti.info.from.file`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.info.from.file.md).

## Note

The data scaling fields of the instance are not applied here, they are
expected to be applied already: instances read by
[`oro.nifti::readNIfTI`](https://rdrr.io/pkg/oro.nifti/man/read_nifti.html)
are rescaled unless `rescale_data = FALSE` was used.

## See also

[`nifti.info.from.file`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti.info.from.file.md)
