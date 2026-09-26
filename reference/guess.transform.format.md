# Determine the format of a transformation file.

Guess the format of a transformation file from its extension and its
content. Content is needed because the extension '.mat' is used by FSL
for text matrices and by ANTs/ITK for binary transformations, which have
nothing in common. A file that is identified as an ITK/ANTs
transformation is reported as such instead of failing with a parse
error, since that format is not supported yet.

## Usage

``` r
guess.transform.format(filepath)
```

## Arguments

- filepath:

  character string, the full path to the transform file.

## Value

character string, the file format, one of 'xfm', 'dat', 'lta' or
'fslmat'.
