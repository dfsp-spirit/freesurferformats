# freesurferformats development information


## Recommended dev environment

* clone the git repo
* install rstudio and R if you do not have them yet
* install the following R packages: `devtools, knitr, testthat`
* install all freesurferformats dependencies, e.g., by installing freesurferformats from CRAN using `install.packages("freesurferformats", dependencies=TRUE);`
* in rstudio, click `File => Open Project` and open the project file `freesurferformats.Rproj` from the root of this repo

## Running the unit tests

In rstudio, click *Build - Test Package*.

On the console:

* to run all tests based on source code in dir: ```Rscript -e "devtools::test()"```
* to run an individual test, or several ones, by name filter: ```Rscript -e "devtools::test(filter = 'write_fs_annot')"```

## Checking the package

This does a lot more than just running the tests, it checks various coding styles, metadata, and all kinds of other stuff that is specific to what the people running CRAN want you to do. It also builds the documentation by default to check whether that works, so it takes a lot of time.

In rstudio, click *Build - Check Package*.

On the console:

* to build package and run all CRAN checks: ```R CMD check```
* to build package and run only package checks and tests (faster): ```R CMD check . --no-manual --no-vignettes```


## Building the documentation (vignettes)

In rstudio, click *Build - Clean and Rebuild*.

On the console, run ```Rscript -e devtools::build_vignettes()```

