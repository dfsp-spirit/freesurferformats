# Check whether filepath ends with extension.

Check whether filepath ends with extension.

## Usage

``` r
filepath.ends.with(filepath, extensions)
```

## Arguments

- filepath:

  string. Path to a file, including filename and extension.

- extensions:

  list of strings. A list of suffixes to check. Case does not matter.
  Example: `extensions=c('.gz', '.mgz')`.

## Value

logical, whether the filepath end with one of the extensions.
