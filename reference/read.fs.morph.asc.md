# Read morphometry data from ASCII curv format file

Read morphometry data from ASCII curv format file

## Usage

``` r
read.fs.morph.asc(filepath)
```

## Arguments

- filepath:

  path to a file in FreeSurfer ASCII curv format. Such a file contains,
  on each line, the following fields, separated by spaces: vertex_index,
  vertex_coord_x, vertex_coord_y, vertex_coord_z, morph_data_value.

## Value

numeric vector, the curv data

## Note

This format is also known as *dpv* (data-per-vertex) format.
