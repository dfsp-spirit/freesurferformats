% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/surface_dist.R
\name{vertexdists.to.point}
\alias{vertexdists.to.point}
\title{Compute Euclidean distance from all mesh vertices to given point.}
\usage{
vertexdists.to.point(surface, point_coords)
}
\arguments{
\item{surface}{an fs.surface instance}

\item{point_coords}{double vector of length 3, the xyz coords of a single point.}
}
\value{
double vector of distances
}
\description{
Compute Euclidean distance from all mesh vertices to given point.
}
\seealso{
Other Euclidean distance util functions: 
\code{\link{closest.vert.to.point}()},
\code{\link{vertex.euclid.dist}()}
}
\concept{Euclidean distance util functions}
