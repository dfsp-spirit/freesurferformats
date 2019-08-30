# freesurferformats development information


## Recommended dev environment

* clone the git repo
* install rstudio
* install the following R packages: `devtools, knitr, testthat`
* in rstudio, open the included project file `freesurferformats.Rproj`

## Running tests

Click *Build - Test Package*

## Checking package

Click *Build - Check Package*

## Building the documentation (vignettes)

Click *Build - Clean and Rebuild*
In the R command window that opens:
```
library("devtools")
build_vignettes()
```
