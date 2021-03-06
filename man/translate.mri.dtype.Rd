% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fs_mgh.R
\name{translate.mri.dtype}
\alias{translate.mri.dtype}
\title{Translate between code and name of MRI data types.}
\usage{
translate.mri.dtype(dtype)
}
\arguments{
\item{dtype}{character string (one of c('MRI_FLOAT') or integer, one of c(0L, 1L, 3L, 4L). Numeric values will be converted to integer.}
}
\value{
if `dtype` is a character string, the respective integer code. If it is numeric, the respective character string.
}
\description{
Translate between code and name of MRI data types.
}
\keyword{internal}
