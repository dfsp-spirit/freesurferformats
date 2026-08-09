# Find files with the given base name and extensions that exist.

Note that in the current implementation, the case of the filepath and
the extension must match.

## Usage

``` r
readable.files(
  filepath,
  precedence = c(".mgh", ".mgz"),
  error_if_none = TRUE,
  return_all = FALSE
)
```

## Arguments

- filepath:

  character string, path to a file without extension

- precedence:

  vector of character strings, the file extensions to check. Must
  include the dot (if you expect one).

- error_if_none:

  logical, whether to raise an error if none of the files exist

- return_all:

  logical, whether to return all readable files instead of just the
  first one

## Value

character string, the path to the first existing file (or `NULL` if none
of them exists).
