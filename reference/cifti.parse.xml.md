# Parse the CIFTI XML metadata.

Parse the CIFTI XML metadata.

## Usage

``` r
cifti.parse.xml(xml_text, filepath = "", niiheader = NULL)
```

## Arguments

- xml_text:

  character string, the XML document.

- filepath:

  character string, the path of the file the XML was read from. Only
  used in error messages.

- niiheader:

  the NIFTI-2 header of the file, required to determine the sizes of the
  matrix dimensions.

## Value

an `fs.cifti` object, see
[`read.cifti.header`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md).
