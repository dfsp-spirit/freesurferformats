# Compute quaternion representation of a rotation from a 4x4 rotation matrix.

Compute quaternion representation of a rotation from a 4x4 rotation
matrix.

## Usage

``` r
m44_to_quaternion(m)
```

## Arguments

- m:

  the input 4x4 matrix encoding the rotation, with homogeneous column
  `[0,0,0,1]`.

## Value

numeric vector of length 4, the quaternion representation (qw, qx, qy,
qz).
