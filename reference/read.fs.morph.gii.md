# Read morphometry data file in GIFTI format.

Read vertex-wise brain surface data from a GIFTI file. The file must be
a GIFTI *func* file (not a GIFTI *surf* file containing a mesh, use
[`read_nisurface`](https://dfsp-spirit.github.io/freesurferformats/reference/read_nisurface.md)
for loading GIFTI surf files).

## Usage

``` r
read.fs.morph.gii(filepath, element_index = 1L)
```

## Arguments

- filepath, :

  string. Full path to the input GIFTI file.

- element_index:

  integer, the element to load in case the GIFTI file containes several
  datasets (usually time series). Defaults to the first element, 1L.

## Value

data, vector of double or integer. The brain morphometry data, one value
per vertex. The data type depends on the data type in the file.

## Note

This function requires the `gifti` package, which is an optional
dependency, to be installed. It also assumes that the dataset contains a
vector or a matrix/array in which all dimensions except for 1 are empty.

## See also

Other morphometry functions:
[`fs.get.morph.file.ext.for.format()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.ext.for.format.md),
[`fs.get.morph.file.format.from.filename()`](https://dfsp-spirit.github.io/freesurferformats/reference/fs.get.morph.file.format.from.filename.md),
[`read.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.curv.md),
[`read.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md),
[`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md),
[`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md),
[`read.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.weight.md),
[`write.fs.curv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.curv.md),
[`write.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.label.gii.md),
[`write.fs.mgh()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.mgh.md),
[`write.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.md),
[`write.fs.morph.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.asc.md),
[`write.fs.morph.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.gii.md),
[`write.fs.morph.ni1()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni1.md),
[`write.fs.morph.ni2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.ni2.md),
[`write.fs.morph.smp()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.smp.md),
[`write.fs.morph.txt()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.txt.md),
[`write.fs.weight()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.md),
[`write.fs.weight.asc()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.weight.asc.md)

Other gifti readers:
[`read.fs.annot.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.annot.gii.md),
[`read.fs.label.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.label.gii.md),
[`read.fs.surface.gii()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.surface.gii.md)
