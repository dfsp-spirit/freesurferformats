# Get the on-disk properties of a VTK legacy data type.

Every data section of a VTK legacy file names the type of its values
explicitly (e.g. `POINTS 8 float`). This function translates such a type
name into the number of bytes a single value occupies on disk, the R
storage mode to read it into, and whether the values are integers.

## Usage

``` r
vtk.data.type.info(type_name, filepath = "")
```

## Arguments

- type_name:

  character string, the type name as it occurs in the file, e.g.
  'float', 'double' or 'vtktypeint64'.

- filepath:

  character string, the file the type name was found in. Only used to
  make error messages more helpful.

## Value

named list with the entries 'size' (integer, bytes per value on disk),
'mode' (character, the storage mode for
[`readBin`](https://rdrr.io/r/base/readBin.html)) and 'integer'
(logical, whether the values are integers).
