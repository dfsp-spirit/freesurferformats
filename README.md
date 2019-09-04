# freesurferformats
GNU R package to read FreeSurfer neuroimaging file formats.

## Supported formats

* MGH/MGZ: FreeSurfer n-dimensional brain images or arbitrary other data. Typically a single 3D brain MRI scan or a time series of scans, or morphometry data for brain surfaces. The format is named after the Massachusetts General Hospital, and the specs are given (rather implicitely) [here in the FreeSurfer wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat). MGZ is just a gzipped version of MGH. An example file would be `mri/T1.mgz` (containing a 3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh` (containing surface data mapped to standard space).

* FreeSurfer 'curv' format: Morphometry data for a brain surface, one scalar per vertex. Could be the thickness or area of the cerebral cortex at each mesh vertex. Several versions of this format exist, the supported version is the new, binary one (the only one that is used in current FS versions). An example file would be `surf/lh.area`.


## Installation

### Recommended: install the stable version from CRAN

The package is on [CRAN](https://CRAN.R-project.org/package=freesurferformats), so you can simply:

```r
install.packages("freesurferformats")
```

### Development version (from GitHub)

You can try the development version if you need features which have not been released yet. Use at your own risk though.

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

Now you can call the following functions:

### *read.fs.mgh* -- Read FreeSurfer MGH or MGZ format files


Let's read a brain volume:

```r
mgz_file = system.file("mystudy", "subject1", "mri", "brain.mgz")
brain_3D_voxels = read.fs.mgh(mgz_file)
```

Now, `brain_3D_voxels` is an *n*-dimensional matrix, where *n* depends on the data in the MGZ file. A conformed FreeSurfer volume like `brain.mgz` typically has 3 dimensions and 256*256*256 = 16777216 voxels.

The MGH/MGZ format is also used to store morphometry data mapped to standard space (fsaverage). Here, we read cortical thickness data in standard space, smoothed with a FWHM 25 kernel:


```r
mgh_file = system.file("mystudy", "subject1", "surf", "lh.thickness.fwhm25.fsaverage.mgh")
cortical_thickness_standard = read.fs.mgh(mgh_file)
```

Now, `cortical_thickness_standard` is a vector of n float values, where *n* is the number of vertices of the fsaverage left hemisphere surface (i.e., 163842 in FreeSurfer 6).


### *read.fs.curv* -- Read FreeSurfer curv format files

```r
curv_file = system.file("mystudy", "subject1", "surf", "lh.thickness")
cortical_thickness_native = read.fs.curv(curv_file)
```

Now, `cortical_thickness_native` is a vector of *n* float values, where *n* is the number of vertices of the surface mesh the data belongs to (usually `surf/lh.white`, the number of vertices differs between subjects).


## Full Documentation

* A short vignette for the package is included, run `vignette("freesurferformats")` or `browseVignettes("freesurferformats")` to see it.
* Help for a specific function can be accessed in the usual R manner: `?function`, where you replace `function` with something like `reaf.fs.mgh`.
* Run `example(function)` to see a live demo that uses the function `function`.


## Unit tests / CI


In a clean R session:

```r
library(devtools)
library(freesurferformats)
devtools::check()
```

Continuous integration is run on travis: [![Build Status](https://travis-ci.org/dfsp-spirit/freesurferformats.svg?branch=master)](https://travis-ci.org/dfsp-spirit/freesurferformats)


## License

[MIT](https://opensource.org/licenses/MIT)

Note: The file LICENSE in this repository is a CRAN license template only (as required by CRAN) and does not contain the full MIT  license text. See the file LICENSE_FULL for the full license text.
