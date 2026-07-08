
## Local checks, Ubuntu 24 LTS

```shell
R CMD build . && R CMD check --as-cran freesurferformats_1.0.1.tar.gz
* checking for file ‘./DESCRIPTION’ ... OK
* preparing ‘freesurferformats’:
* checking DESCRIPTION meta-information ... OK
* installing the package to build vignettes
* creating vignettes ... OK
* checking for LF line-endings in source and make files and shell scripts
* checking for empty or unneeded directories
Removed empty directory ‘freesurferformats/tests/testdata’
Removed empty directory ‘freesurferformats/web’
* building ‘freesurferformats_1.0.1.tar.gz’

* using log directory ‘/home/ts/develop/freesurferformats/freesurferformats.Rcheck’
* using R version 4.3.3 (2024-02-29)
* using platform: x86_64-pc-linux-gnu (64-bit)
* R was compiled by
    gcc (Ubuntu 13.2.0-23ubuntu3) 13.2.0
    GNU Fortran (Ubuntu 13.2.0-23ubuntu3) 13.2.0
* running under: Ubuntu 24.04.4 LTS
* using session charset: UTF-8
* using option ‘--as-cran’
* checking for file ‘freesurferformats/DESCRIPTION’ ... OK
* checking extension type ... Package
* this is package ‘freesurferformats’ version ‘1.0.1’
* package encoding: UTF-8
* checking CRAN incoming feasibility ... [4s/17s] Note_to_CRAN_maintainers
Maintainer: ‘Tim Schäfer <ts+code@rcmd.org>’
* checking package namespace information ... OK
* checking package dependencies ... OK
* checking if this is a source package ... OK
* checking if there is a namespace ... OK
* checking for executable files ... OK
* checking for hidden files and directories ... OK
* checking for portable file names ... OK
* checking for sufficient/correct file permissions ... OK
* checking serialization versions ... OK
* checking whether package ‘freesurferformats’ can be installed ... OK
* checking installed package size ... OK
* checking package directory ... OK
* checking for future file timestamps ... NOTE
unable to verify current time
* checking ‘build’ directory ... OK
* checking DESCRIPTION meta-information ... OK
* checking top-level files ... OK
* checking for left-over files ... OK
* checking index information ... OK
* checking package subdirectories ... OK
* checking R files for non-ASCII characters ... OK
* checking R files for syntax errors ... OK
* checking whether the package can be loaded ... OK
* checking whether the package can be loaded with stated dependencies ... OK
* checking whether the package can be unloaded cleanly ... OK
* checking whether the namespace can be loaded with stated dependencies ... OK
* checking whether the namespace can be unloaded cleanly ... OK
* checking loading without being on the library search path ... OK
* checking use of S3 registration ... OK
* checking dependencies in R code ... OK
* checking S3 generic/method consistency ... OK
* checking replacement functions ... OK
* checking foreign function calls ... OK
* checking R code for possible problems ... OK
* checking Rd files ... OK
* checking Rd metadata ... OK
* checking Rd line widths ... OK
* checking Rd cross-references ... OK
* checking for missing documentation entries ... OK
* checking for code/documentation mismatches ... OK
* checking Rd \usage sections ... OK
* checking Rd contents ... OK
* checking for unstated dependencies in examples ... OK
* checking installed files from ‘inst/doc’ ... OK
* checking files in ‘vignettes’ ... OK
* checking examples ... OK
* checking for unstated dependencies in ‘tests’ ... OK
* checking tests ...
  Running ‘testthat.R’ [19s/18s]
 [20s/18s] OK
* checking for unstated dependencies in vignettes ... OK
* checking package vignettes in ‘inst/doc’ ... OK
* checking re-building of vignette outputs ... OK
* checking PDF version of manual ... OK
* checking HTML version of manual ... OK
* checking for non-standard things in the check directory ... OK
* checking for detritus in the temp directory ... OK
* DONE

Status: 1 NOTE
See
  ‘/home/ts/develop/freesurferformats/freesurferformats.Rcheck/00check.log’
for details.
```


## Other

Checked on Winbuilder via upload form, all 3 R versions say OK.



