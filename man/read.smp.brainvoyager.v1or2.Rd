% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/brainvoyager.R
\name{read.smp.brainvoyager.v1or2}
\alias{read.smp.brainvoyager.v1or2}
\title{Read Brainvoyager statistical surface results from v1 or v2 SMP file.}
\usage{
read.smp.brainvoyager.v1or2(filepath, version)
}
\arguments{
\item{filepath}{character string, path to file in Brainvoyager SMP file format}
}
\value{
named list of file contents
}
\description{
Read Brainvoyager statistical surface results from v1 or v2 SMP file.
}
\note{
Do not call this, call \code{read.smp.brainvoyager} instead, which will figure out the version and call the appropriate function.
}
\keyword{internal}
