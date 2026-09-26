# Write the 1000 byte header of a TRK file.

Write the 1000 byte header of a TRK file.

## Usage

``` r
write.trk.header(con, header, num_tracks, endian)
```

## Arguments

- con:

  a connection opened in binary write mode.

- header:

  named list, the header fields to write. Missing fields get defaults.

- num_tracks:

  integer, the value for the 'n_count' field.

- endian:

  character string, 'little' or 'big'.

## Value

`NULL`, invisibly. The header is written to `con`.
