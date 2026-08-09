# Write fixed width integers to one or several lines.

Write fixed width integers to one or several lines.

## Usage

``` r
fixed.vec.format.int(
  vdata,
  num_chars_per_entry,
  max_entries_per_line = NULL,
  align_right = TRUE
)
```

## Arguments

- vdata:

  integer vector, the data

- num_chars_per_entry:

  field length of a single formatted integer in characters

- max_entries_per_line:

  integer, how many entries are allowed per line. Leave at NULL for no
  limit, which will return all in a single line.

- align_right:

  logical, whether to align the integers to the right. As you may have
  guessed, set to `FALSE` to align to the left.

## Value

vector of character strings, the formatted data lines.
