# Read binary colortable in old format.

Read an oldformat colortable from a connection to a binary file.

## Usage

``` r
readcolortable_oldformat(fh, ctable_num_entries)
```

## Arguments

- fh:

  file handle

- ctable_num_entries:

  number of entries to read

## Value

named list, the color table. The named entries are: "num_entries": int,
number of brain structures. "struct_names": vector of strings, the brain
structure names. "table": numeric matrix with num_entries rows and 5
colums. The 5 columns are: 1 = color red channel, 2=color blue channel,
3=color green channel, 4=color alpha channel, 5=unique color code.
