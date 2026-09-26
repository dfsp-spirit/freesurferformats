# Format a single numerical value for a transformation text file.

17 significant decimal digits identify a double uniquely, so writing
that many of them and reading them back preserves the value exactly –
but only if the decimal conversion of the C library rounds correctly,
and that is not true on every platform: on macOS (ARM64) the round trip
of a value like `-1e-7 / 7` loses one unit in the last place, because
the value that is written is one digit off. The round trip is therefore
verified here with R's own decimal conversion, and the number of digits
is increased until the value survives it. The output therefore has no
more digits than the platform can handle correctly, and it is guaranteed
to be read back exactly by the same platform.

## Usage

``` r
# S3 method for class 'value.text'
transform(value)
```

## Arguments

- value:

  single numerical value, the value to format.

## Value

character string, the text representation of the value.
