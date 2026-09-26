# Read the matrices of a MATLAB v4 data file.

MATLAB v4 files (the format behind the `.mat` files that SPM and
FreeSurfer write next to an ANALYZE image, see
[`read.fs.volume.analyze`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md))
are the simplest MATLAB data format: a small header with the type, the
dimensions, the imaginary flag and the variable name, followed by the
raw values in column major order. This function reads the matrices of
such a file.

## Usage

``` r
read.matlab.v4.matrix(filepath)
```

## Arguments

- filepath:

  character string, the path of the file to read.

## Value

named list of matrices, one per variable in the file (a v4 file may
contain several variables, which are stored one after another). The
names are the variable names (e.g. `'M'` or `'mat'`), and the values are
matrices of the stored dimensions. Returns `NULL` if the file is not a
MATLAB v4 file, i.e. if no interpretation of its content leads to a file
whose size matches the declared variables. Other MATLAB file versions
(v5 and newer, which are compressed and have a completely different
structure) are reported as `NULL` as well, instead of returning data
from a wrongly interpreted header.

## Note

Only the numeric types of the format are read (double, float, int32,
int16, uint16 and uint8, which are the type codes 0, 10, 20, 30, 40 and
50). Text matrices and variables with more than 2 dimensions are not
supported, the latter because the v4 format cannot store them.
