# Resolve the data location of a NRRD file.

Determines where the data of a NRRD file are stored: in the file itself,
in a file named by the 'data file' header field (a path relative to the
directory of the header file), or in the several files that the 'data
file: LIST' mode names.

## Usage

``` r
nrrd.resolve.data.files(fields, data_file_names, filepath)
```

## Arguments

- fields:

  named list, the parsed header fields.

- data_file_names:

  character vector or NULL, the file names of the LIST mode.

- filepath:

  character string, path to the header file.

## Value

character vector of data file paths, or NULL when the data are attached
to the header file.
