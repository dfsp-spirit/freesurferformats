# Read the header extensions of a NIFTI v2 file.

Read the header extensions of a NIFTI v2 file.

## Usage

``` r
nifti2.read.extensions(fh, available, endian = "little")
```

## Arguments

- fh:

  filehandle, a connection to a NIFTI v2 file, positioned at the first
  byte after the fixed-size header (i.e., at the extension flag bytes).

- available:

  integer, the number of bytes available for header extensions, i.e.,
  `vox_offset` minus the 544 bytes of the fixed-size header and the
  extension flag.

- endian:

  character string, the endianness of the file, either 'little' or
  'big'.

## Value

list of header extensions, see
[`nifti2.extension`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md).
Empty list if the file has no extensions.
