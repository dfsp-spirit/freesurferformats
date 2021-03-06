% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_fs_volume.R
\name{read.fs.volume}
\alias{read.fs.volume}
\title{Read volume file in MGH, MGZ or NIFTI format}
\usage{
read.fs.volume(
  filepath,
  format = "auto",
  flatten = FALSE,
  with_header = FALSE,
  drop_empty_dims = FALSE
)
}
\arguments{
\item{filepath}{string. Full path to the input MGZ, MGH or NIFTI file.}

\item{format}{character string, one one of 'auto', 'nii', 'mgh' or 'mgz'. The format to assume. If set to 'auto' (the default), the format will be derived from the file extension.}

\item{flatten}{logical. Whether to flatten the return volume to a 1D vector. Useful if you know that this file contains 1D morphometry data.}

\item{with_header}{logical. Whether to return the header as well. If TRUE, return an instance of class `fs.volume` for data with at least 3 dimensions, a named list with entries "data" and "header". The latter is another named list which contains the header data. These header entries exist: "dtype": int, one of: 0=MRI_UCHAR; 1=MRI_INT; 3=MRI_FLOAT; 4=MRI_SHORT. "voldim": integer vector. The volume (=data) dimensions. E.g., c(256, 256, 256, 1). These header entries may exist: "vox2ras_matrix" (exists if "ras_good_flag" is 1), "mr_params" (exists if "has_mr_params" is 1). See the `mghheader.*` functions, like \code{\link[freesurferformats]{mghheader.vox2ras.tkreg}}, to compute more information from the header fields.}

\item{drop_empty_dims}{logical, whether to drop empty dimensions of the returned data}
}
\value{
data, multi-dimensional array. The brain imaging data, one value per voxel. The data type and the dimensions depend on the data in the file, they are read from the header. If the parameter flatten is `TRUE`, a numeric vector is returned instead. Note: The return value changes if the parameter with_header is `TRUE`, see parameter description.
}
\description{
Read multi-dimensional brain imaging data from a file.
}
\examples{
    brain_image = system.file("extdata", "brain.mgz",
                               package = "freesurferformats",
                               mustWork = TRUE);
    vd = read.fs.volume(brain_image);
    cat(sprintf("Read voxel data with dimensions \%s. Values: min=\%d, mean=\%f, max=\%d.\n",
                 paste(dim(vd), collapse = ' '), min(vd), mean(vd), max(vd)));
    # Read it again with full header data:
    vdh = read.fs.volume(brain_image, with_header = TRUE);
    # Use the vox2ras matrix from the header to compute RAS coordinates at CRS voxel (0, 0, 0):
    vox2ras_matrix = mghheader.vox2ras(vdh)
    vox2ras_matrix \%*\% c(0,0,0,1);

}
\seealso{
To derive more information from the header, see the `mghheader.*` functions, like \code{\link[freesurferformats]{mghheader.vox2ras.tkreg}}.

Other morphometry functions: 
\code{\link{fs.get.morph.file.ext.for.format}()},
\code{\link{fs.get.morph.file.format.from.filename}()},
\code{\link{read.fs.curv}()},
\code{\link{read.fs.mgh}()},
\code{\link{read.fs.morph.gii}()},
\code{\link{read.fs.morph}()},
\code{\link{read.fs.weight}()},
\code{\link{write.fs.curv}()},
\code{\link{write.fs.label.gii}()},
\code{\link{write.fs.mgh}()},
\code{\link{write.fs.morph.asc}()},
\code{\link{write.fs.morph.gii}()},
\code{\link{write.fs.morph.ni1}()},
\code{\link{write.fs.morph.ni2}()},
\code{\link{write.fs.morph.smp}()},
\code{\link{write.fs.morph.txt}()},
\code{\link{write.fs.morph}()},
\code{\link{write.fs.weight.asc}()},
\code{\link{write.fs.weight}()}
}
\concept{morphometry functions}
