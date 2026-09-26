# Refuse a CIFTI-2 file name for a file that is not a CIFTI-2 file.

The writers of this package that are not CIFTI writers
([`write.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.md),
[`write.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.volume.md))
derive the format from the file name, and a CIFTI-2 file name looks like
a NIFTI name. Writing a NIFTI file under a name like `.dscalar.nii`
would produce a file whose content contradicts its name: this package
(and Connectome Workbench) refuse to read it, because the name promises
the CIFTI XML metadata that describes what the matrix dimensions
contain. This function turns that into an error that names the writer to
use instead.

## Usage

``` r
cifti.stop.if.cifti.name(filepath)
```

## Arguments

- filepath:

  character string, the name of the file that is about to be written.

## Value

`NULL`, invisibly. Stops if the name is one of the standard CIFTI-2 file
names.
