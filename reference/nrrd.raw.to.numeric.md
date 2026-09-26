# Convert raw bytes of wide integer types into numbers.

R's `readBin` can read integers of 1, 2 and 4 bytes, but an unsigned 4
byte integer does not fit into R's signed integer type and there is no
integer type of 8 bytes at all, so these types are interpreted here from
their raw bytes. The values are computed in double precision, which is
exact up to 2^53 (for the 64 bit types, larger values lose their low
bits, see the note in `read.fs.volume.nrrd`).

## Usage

``` r
nrrd.raw.to.numeric(bytes, type_info, endian)
```

## Arguments

- bytes:

  raw vector, the bytes of the values.

- type_info:

  named list, the R data type information, see `nrrd.type.info`.

- endian:

  character string, 'little' or 'big'.

## Value

numeric vector, one value per 4 or 8 bytes.
