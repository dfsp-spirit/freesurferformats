# Parse the header lines of a NRRD file.

Splits the header lines into an NRRD magic line and a named list of
typed field values. Field names are normalized for lookup (see
`nrrd.field.key`), the original spelling is kept in the result. A field
may be written as 'name: value' or as 'name:=value', the latter meaning
that the value is a string, which is how the tools that write custom
fields (e.g. the DWI metadata, or pynrrd) store them. The special 'data
file: LIST' mode is handled as well: the lines that follow it, up to the
end of the header, are the names of the data files.

## Usage

``` r
nrrd.parse.header(lines, filepath = "")
```

## Arguments

- lines:

  character vector, the header lines, see `nrrd.read.header.lines`.

- filepath:

  character string, path to the file, used in error messages.

## Value

named list with entries `magic` (character string), `fields` (named
list, keyed by the normalized field name), `field_names` (named
character vector, the original spelling per key) and `data_file_names`
(character vector, the file names of the LIST mode, or NULL).
