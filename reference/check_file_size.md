# Check that a file is large enough to contain the expected data payload.

Compares the expected data size against the file size on disk. Stops
with an error if the file is too small (truncated or corrupt).

## Usage

``` r
check_file_size(filepath, header_bytes, data_bytes)
```

## Arguments

- filepath:

  character string, path to the file.

- header_bytes:

  single numeric value, the number of bytes consumed by the file header
  (everything before the data payload).

- data_bytes:

  single numeric value, the expected number of bytes in the data
  payload.

## Value

logical, `TRUE` (invisibly) if the file is large enough. Stops with an
error otherwise.
