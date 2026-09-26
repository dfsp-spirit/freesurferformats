# Group axes into MatrixIndicesMap elements.

A CIFTI-2 file describes its matrix dimensions with `MatrixIndicesMap`
elements, and the format says that the same element must be used for
both dimensions when they describe the same thing. This is the case for
connectome files (a `.dconn` or `.pconn` has the same brainordinates or
parcels in both dimensions), and Connectome Workbench writes such a file
with a single element that applies to both dimensions, while nibabel
writes one per dimension. The merged form is what this writer produces.

## Usage

``` r
cifti.axes.to.maps(axes)
```

## Arguments

- axes:

  list of axes, named by matrix dimension.

## Value

a list of lists with the entries 'axis' (the axis) and 'dims' (the
matrix dimensions it describes).
