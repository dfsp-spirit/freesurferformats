# freesurferformats development information


## Recommended dev environment

* clone the git repo
* install rstudio
* install the following R packages: `devtools, knitr, testthat`
* in rstudio, open the included project file `freesurferformats.Rproj`

## Running tests

In rstudio, click *Build - Test Package*.

On the console:

* to run all tests based on source code in dir: ```Rscript -e "devtools::test()"```
* to run an individual test, or several ones, by name filter: ```Rscript -e "devtools::test(filter = 'write_fs_annot')"```

## Checking package

In rstudio, click *Build - Check Package*.

On the console:

* to build package and run all CRAN checks: ```R CMD check```
* to build package and run only tests (faster): ```R CMD check . --no-manual --no-vignettes```


## Building the documentation (vignettes)

In rstudio, click *Build - Clean and Rebuild*.

On the console, ```devtools::build_vignettes()```

