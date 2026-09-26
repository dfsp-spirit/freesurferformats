# Convert a transformation between the LPS and the RAS convention.

Changing the convention of the coordinates that a transformation maps
flips the sign of its first two axes on each side of the transformation
that changes: the input side by multiplying the matrix from the right
and the output side by multiplying it from the left, with a diagonal
matrix that negates x and y. That matrix is its own inverse, which is
why the conversion in both directions is the same operation.

## Usage

``` r
# S3 method for class 'flip.handedness'
transform(tf, target)
```

## Arguments

- tf:

  an `fs.transform` instance.

- target:

  character string, either 'ras' or 'lps'.

## Value

an `fs.transform` instance whose matrix is expressed in the requested
convention.
