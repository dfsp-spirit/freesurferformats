% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fs_surface.R
\name{parse.stl.ascii.face}
\alias{parse.stl.ascii.face}
\title{Parse a single ASCII STL face.}
\usage{
parse.stl.ascii.face(stl_face_lines)
}
\arguments{
\item{stl_face_lines}{vector of exactly 7 character strings, the lines from an STL ASCII file defining a triangular face.}
}
\value{
named list with entries: 'face_normal': double matrix with 1 row and 3 columns, the face normal. 'vertex_coords': double matrix with 3 rows and 3 columns, the 3x3 vertex coordinates of the face, each row contain the x, y, and z coordinate of a vertex.
}
\description{
Parse a single ASCII STL face.
}
\keyword{internal}
