# Write the header extensions of a NIFTI v2 file.

Write the header extensions of a NIFTI v2 file.

## Usage

``` r
nifti2.write.extensions(fh, extensions, endian = "little")
```

## Arguments

- fh:

  filehandle, a connection to a NIFTI v2 file, positioned at the first
  byte after the fixed-size header (i.e., where the extension flag bytes
  go).

- extensions:

  list of header extensions, see
  [`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).
  An empty list writes the extension flag for 'no extensions present'.

- endian:

  character string, the endianness of the file, either 'little' or
  'big'.

## Value

integer, the number of bytes written (the extension flag bytes plus all
extensions). The data must start at `544 + <return value>`.
