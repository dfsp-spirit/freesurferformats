# Check whether a file is a CIFTI-2 file (internal helper).

Cheap and silent check whether a file is a CIFTI-2 file, i.e. a NIFTI v2
file with a header extension of code 32, the extension that holds the
CIFTI XML metadata. The format dispatchers
([`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md))
use it to give a helpful error: the payload of a CIFTI-2 file is a
matrix whose dimensions the XML describes, not a volume or a per-vertex
vector, so reading it with the NIFTI reader silently returns values in
an order that means nothing.

## Usage

``` r
cifti.file.looks.like.cifti2(filepath)
```

## Arguments

- filepath:

  character string, the path to a CIFTI-2 file (usually one of
  `.dscalar.nii`, `.dtseries.nii`, `.dlabel.nii`, `.dconn.nii`,
  `.pscalar.nii`, `.ptseries.nii`, `.pconn.nii`, `.dpconn.nii` or
  `.pdconn.nii`). Note that this is not a NIFTI file, despite the `.nii`
  part. Gzipped CIFTI files are not supported, because the CIFTI-2
  format forbids compression.

## Value

logical, whether the file is a CIFTI-2 file. A file that does not exist
or cannot be parsed is reported as `FALSE`.
