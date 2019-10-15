# freesurferformats
GNU R package to read and write FreeSurfer neuroimaging file formats.

![Vis](./vignettes/rgl_brain_aparc.jpg?raw=true "aparc atlas visualization")

## Supported formats

* MGH/MGZ: FreeSurfer n-dimensional brain images or arbitrary other data. Typically a single 3D brain MRI scan or a time series of scans, or morphometry data for brain surfaces. The format is named after the Massachusetts General Hospital, and the specs are given (rather implicitely) [here in the FreeSurfer wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat). MGZ is just a gzipped version of MGH. An example file would be `mri/T1.mgz` (containing a 3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh` (containing surface data mapped to standard space). This format can be read and written. Reading and writing header data is also supported, and the ras2vox matrix is computed from the header data.

* FreeSurfer 'curv' format: Morphometry data for a brain surface, one scalar per vertex. Could be the thickness or area of the cerebral cortex at each mesh vertex. Several versions of this format exist, the supported version is the new, binary one (the only one that is used in current FS versions). An example file would be `surf/lh.area`. This format can be read and written.

* FreeSurfer annotation file format: Contains a cortical parcellation. A cortical parcellation originates from a brain atlas and contains a label for each vertex of a surface that assigns this vertex to one of a set of atlas regions. The file format also contains a colortable, which assigns a color code to each atlas region. An example file would be `labels/lh.aparc.annot`. This format can only be read.

* FreeSurfer surface file format: Contains a brain surface mesh. Such a mesh is defined by a list of vertices (each vertex is given by its x,y,z coords) and a list of faces (each face is given by three vertex indices). Currently only triangular meshes are implemented. An example file would be `surf/lh.white`. This format can be read and written.

* FreeSurfer label file format: Contains a list of vertices included in a label. A label is like a mask, and is typically used to describe the vertices which are part of a certain brain region. An example file would be `label/lh.cortex.label`. This format can be read.


## Installation

### Recommended: install the stable version from CRAN

The package is on [CRAN](https://CRAN.R-project.org/package=freesurferformats), so you can simply:

```r
install.packages("freesurferformats")
```

### Development version (from GitHub)

You can try the development version if you need features which have not been released yet. Use at your own risk though, development is currently happending on master and the chance of grabbing a broken version is real. Please run the tests before using the dev version (see the *Unit tests / CI* section below).

If you do not have `devtools` and related tools installed yet:

```r
install.packages(c("devtools", "knitr", "testthat"));
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
read.fs.surface() -- read a surface mesh

write.fs.mgh() -- write data with 1 to 4 dimensions to an MGH format file
write.fs.curv() -- write a data vector to a 'curv' format file
write.fs.morph() -- wrapper that writes any morphometry file (mgh/mgz/curv). The format is derived from the file extension.
write.fs.surface() -- write a surface mesh
```

The documentation is included in the package and not repeated on this website.

## Full Documentation

The documentation can be accessed from within an R session after you have loaded the *freesurferformats* package:

* Detailed vignettes with explanations and examples for the usage of all functions of the package are included, run `browseVignettes("freesurferformats")` to see them. You can also open the vignettes directly:
  * learn how to read neuroimaging data: `vignette("freesurferformats")` [read online at CRAN](https://cran.r-project.org/web/packages/freesurferformats/vignettes/freesurferformats.html)
  * learn how to write neuroimaging data: `vignette("freesurferformats_write")` [read online at CRAN](https://cran.r-project.org/web/packages/freesurferformats/vignettes/freesurferformats_write.html) 
* Help for a specific function can be accessed in the usual R manner: `?<function>`, where you replace `<function>` with a function name. Like this: `?read.fs.mgh`.
* Run `example(<function>)` to see a live demo that uses the function `<function>`. Like this: `example(read.fs.mgh)`.


## An example R session: Reading Bert's brain

One of the example subjects that comes with FreeSurfer is `bert`. The following example shows how to load Bert's brain. If you have FreeSurfer installed, you can start GNU R by typing `R` in your favourite terminal application and run the following commands:

```r
install.packages("freesurferformats")

# Load volume from file
library("freesurferformats")
berts_brain = paste(Sys.getenv("FREESURFER_HOME"), "/subjects/bert/mri/brain.mgz", sep="")
mgh = read.fs.mgh(berts_brain, with_header=TRUE);

# Inspect the header:
mgh$header$vox2ras_matrix
#     [,1] [,2] [,3]      [,4]
#[1,]   -1    0    0  133.3997
#[2,]    0    0    1 -110.0000
#[3,]    0   -1    0  128.0000
#[4,]    0    0    0    1.0000

# ...and the data:
mean(mgh$data)
#[1] 8.214322
dim(drop(mgh$data))
# [1] 256 256 256
```



## Unit tests / CI


In a clean R session:

```r
library(devtools)
library(freesurferformats)
devtools::check()
```

Continuous integration is run on travis: [![Build Status](https://travis-ci.org/dfsp-spirit/freesurferformats.svg?branch=master)](https://travis-ci.org/dfsp-spirit/freesurferformats)

[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/freesurferformats?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/freesurferformats)

Don't worry if you are using the stable version from CRAN and CI is currently failing, development happens on master.


## License

[MIT](https://opensource.org/licenses/MIT)

Note: The file LICENSE in this repository is a CRAN license template only (as required by CRAN) and does not contain the full MIT  license text. See the file LICENSE_FULL for the full license text.
