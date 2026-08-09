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

* to run CRAN checks on source: ```Rscript -e "devtools::check()"```
* to build package and run CRAN checks on build version: ```R CMD build . && R CMD check freesurferformats_1.0.0.tar.gz```, or whatever version your are building
* to build package and run only package checks and tests (faster): ```R CMD check . --no-manual --no-vignettes```
* run the hard-core way before a release, so you do not get bothered by CRAN later: ```R CMD check --as-cran```

Observe the output of those check commands carefully, they skip checks if a tool is not installed locally. E.g., to get all checks, you may need to install these:

```sudo apt install pqdf tidy```

## Building the documentation (vignettes)

In rstudio, click *Build - Clean and Rebuild*.

On the console, run ```Rscript -e devtools::build_vignettes()```


## Building the function documentation from inline doc strings in the code

You will need to do this if you added a new argument to a function and R CMD check complains about code/documentation mismatches.

On the console, run ```Rscript -e "roxygen2::roxygenise()"```


## Making a new release

- Make sure all changes are logged in CHANGES file
- Bump version in DESCRIPTION
- Ensure you re-generated function doc strings etc, e.g., via `devtools::document()`
- Build package and make sure it passes CRAN tests locally. Best done with a recent R version, as they may have introduced even more annoying checks in later versions: ```R CMD check build . && R CMD check --as-cran freesurferformats_1.0.0.tar.gz```, or whatever version your are building
- Upload the package to [winbuilder](https://win-builder.r-project.org/upload.aspx) to check there. The service will read package metadata for your email and report back via mail when done.
- If everything is green both locally and on Winbuilder, submit to CRAN via their [package submission form](https://cran.r-project.org/submit.html)
- You will receive feedback from CRAN, either package was accepted or some version of R they test with some check still failed. Bad luck. You will have to modify source and do the loop again.
- Once it passes and CRAN confirms it's on its way to the repo, tag the final git submit that made it into CRAN with the version, e.g. ```git tag v1.0.0 c2hf5hjdk3``` if `c2hf5hjdk3` is the commit ID. Check ```git log --oneline``` for commit IDs. When you have tagged it like this locally, make sure to push the tag: ```git push --tags```.
- Log into github.com, and make a release there based on the tag. Copy relevant CHANGES section as description.


## Continuous integration results:

<!-- badges: start -->
[![R-CMD-check](https://github.com/dfsp-spirit/freesurferformats/workflows/R-CMD-check/badge.svg)](https://github.com/dfsp-spirit/freesurferformats/actions)


[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/dfsp-spirit/freesurferformats?branch=master&svg=true)](https://ci.appveyor.com/project/dfsp-spirit/freesurferformats) AppVeyor CI under Windows

[![codecov](https://codecov.io/gh/dfsp-spirit/freesurferformats/branch/master/graph/badge.svg)](https://codecov.io/gh/dfsp-spirit/freesurferformats) Test coverage
<!-- badges: end -->

The displayed status represents the development version. Don't worry if you are using the stable version from CRAN and CI is currently failing.

### Contributing

If you found a bug, have any question, suggestion or comment on freesurferformats, please [open an issue](https://github.com/dfsp-spirit/freesurferformats/issues). I will definitely answer and try to help.

Please see [CONTRIBUTING.md](CONTRIBUTING.md) for instructions on how to contribute code.


## Author and Contact

The freesurferformats package was written by [Tim Schäfer](https://ts.rcmd.org). To contact me in person, please use the maintainer email address listed on the [CRAN webpage for freesurferformats](https://cran.r-project.org/package=freesurferformats).
