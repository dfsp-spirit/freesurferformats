% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fs_surface.R
\name{assert.surface}
\alias{assert.surface}
\title{Stop unless surf is an fs.surface}
\usage{
assert.surface(surface, param_name = "surface")
}
\arguments{
\item{param_name}{character string, used in stop message to identify the parameter.}

\item{surf}{fs.surface instance or anything else}
}
\value{
Called for the side effect of stopping if surf is not an fs.surface instance.
}
\description{
Stop unless surf is an fs.surface
}
\keyword{internal}
