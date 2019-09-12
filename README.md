# freesurferformats
GNU R package to read and write FreeSurfer neuroimaging file formats.

## Supported formats

* MGH/MGZ: FreeSurfer n-dimensional brain images or arbitrary other data. Typically a single 3D brain MRI scan or a time series of scans, or morphometry data for brain surfaces. The format is named after the Massachusetts General Hospital, and the specs are given (rather implicitely) [here in the FreeSurfer wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat). MGZ is just a gzipped version of MGH. An example file would be `mri/T1.mgz` (containing a 3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh` (containing surface data mapped to standard space). This format can be read and written. Reading and writing header data (mr_parms, ras2vox matrix) is also supported.

* FreeSurfer 'curv' format: Morphometry data for a brain surface, one scalar per vertex. Could be the thickness or area of the cerebral cortex at each mesh vertex. Several versions of this format exist, the supported version is the new, binary one (the only one that is used in current FS versions). An example file would be `surf/lh.area`. This format can be read and written.

* FreeSurfer annotation file format: Contains a cortical parcellation. A cortical parcellation originates from a brain atlas and contains a label for each vertex of a surface that assigns this vertex to one of a set of atlas regions. The file format also contains a colortable, which assigns a color code to each atlas region. An example file would be `labels/lh.aparc.annot`. This format can only be read.


## Installation

### Recommended: install the stable version from CRAN

The package is on [CRAN](https://CRAN.R-project.org/package=freesurferformats), so you can simply:

```r
install.packages("freesurferformats")
```

### Development version (from GitHub)

You can try the development version if you need features which have not been released yet. Use at your own risk though, development is currently happending on master and the chance of grabbing a broken version is real. Please run the tests before using the dev version (see the *Unit tests / CI* section below).

If you do not have `devtools` installed and loaded yet:

```r
install.packages("devtools")
library("devtools")
```

Then:

```r
devtools::install_github("dfsp-spirit/freesurferformats", build_vignettes=TRUE)
```


## Usage

Before using any functions, of course load the package itself:

```r
library("freesurferformats")
```

Now you can call the following functions (list reflects the dev version):


```r
read.fs.mgh() -- read volume or morphometry data from files in MGH or MGZ format, e.g., `mri/brain.mgz` or `surf/lh.area.fwhm10.fsaverage.mgh`.
read.fs.curv() -- read morphometry data from 'curv' format files like `surf/lh.area`
read.fs.morph() -- wrapper that reads any morphometry file (mgh/mgz/curv). The format is derived from the file extension.
read.fs.annot() -- read annotation data or brain atlas labels from files like `label/lh.aparc.annot`

write.fs.mgh() -- write data with 1 to 4 dimensions to an MGH format file
write.fs.curv() -- write a data vector to a 'curv' format file
write.fs.morph() -- wrapper that writes any morphometry file (mgh/mgz/curv). The format is derived from the file extension.
```

The documentation is included in the package and not repeated on this website.

## Full Documentation

* A detailed vignette with explanation and examples for the usage of all functions of the package is included, run `vignette("freesurferformats")` or `browseVignettes("freesurferformats")` to see it.
* Help for a specific function can be accessed in the usual R manner: `?function`, where you replace `function` with something like `read.fs.mgh`.
* Run `example(function)` to see a live demo that uses the function `function`.


## Unit tests / CI


In a clean R session:

```r
library(devtools)
library(freesurferformats)
devtools::check()
```

Continuous integration is run on travis: [![Build Status](https://travis-ci.org/dfsp-spirit/freesurferformats.svg?branch=master)](https://travis-ci.org/dfsp-spirit/freesurferformats)

Don't worry if you are using the stable version from CRAN and CI is currently failing, development happens on master.


## License

[MIT](https://opensource.org/licenses/MIT)

Note: The file LICENSE in this repository is a CRAN license template only (as required by CRAN) and does not contain the full MIT  license text. See the file LICENSE_FULL for the full license text.
