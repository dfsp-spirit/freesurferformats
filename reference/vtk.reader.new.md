# Create the low-level state used while reading a VTK legacy file.

Opens a connection to the file (transparently handling gzip compression)
and initializes the lookahead buffer. The returned environment is passed
to the other `vtk.*` functions, which mutate its state while they
consume the file.

## Usage

``` r
vtk.reader.new(filepath)
```

## Arguments

- filepath:

  character string, path to the VTK file.

## Value

an environment with the class 'vtk.reader'.
