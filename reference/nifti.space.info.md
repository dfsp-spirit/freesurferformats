# Compute NIFTI space unit info from xyzt_units header field.

Compute NIFTI space unit info from xyzt_units header field.

## Usage

``` r
nifti.space.info(xyzt_units)
```

## Arguments

- xyzt_units:

  a single character, the `xyzt_units` NIFTI header field

## Value

named list with entries: `code`: the NIFTI unit code as a decimal
integer, `name`: character string, the unit name, `scaling`: float, the
scaling factor for the unit, relative to the FreeSurfer space unit
(`mm`).
