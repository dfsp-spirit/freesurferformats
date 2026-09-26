# Find the b-value file that belongs to a b-vector file.

Looks for the `bvals` file next to a `bvecs` file, by replacing the
`bvec`/`bvecs` part of the file name with `bval`/`bvals`, and by
replacing the file extension if that does not match. This makes the
readers work out of the box on standard datasets, e.g. the BIDS files
`sub-01_dwi.bvec` and `sub-01_dwi.bval`.

## Usage

``` r
.find.bval.file(bvec_filepath)
```

## Arguments

- bvec_filepath:

  character string, path to the b-vector file.

## Value

character string, the path to the existing b-value file, or NULL if none
of the candidates exists.
