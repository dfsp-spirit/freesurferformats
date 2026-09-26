# freesurferformats

GNU R package to read and write structural neuroimaging file formats.
Comes with support for file formats used by
[FreeSurfer](https://freesurfer.net/),
[CAT12](http://www.neuro.uni-jena.de/cat/)/[SPM](https://www.fil.ion.ucl.ac.uk/spm/software/),
[FSL](https://fsl.fmrib.ox.ac.uk/fsl/fslwiki),
[BrainVoyager](http://www.brainvoyager.com/),
[MRtrix3](https://www.mrtrix.org/), [Diffusion
Toolkit/TrackVis](http://trackvis.org/dtk/) and other neuroimaging
software packages.

![Vis](./articles/rgl_brain_aparc.jpg?raw=true "An aparc brain atlas visualization, created with the fsbrain R package.")

Vis

[Supported formats](#supported-formats) \| [Installation](#installation)
\| [Documentation](#documentation) \| [License](#license) \|
[Citation](#citation) \| [Development](#development)

## A note to end users

This low-level package provides well-tested file format readers and
writers for [FreeSurfer](http://freesurfer.net) neuroimaging data.
Typically, you want to access not only individual files, but datasets of
subjects stored in the standardized output structure of recon-all (your
\$SUBJECTS_DIR) when doing neuroimaging research. In that case, I
recommend to use the high-level functions from the [fsbrain
package](https://github.com/dfsp-spirit/fsbrain) instead of re-inventing
the wheel. The *fsbrain* package is built on top of *freesurferformats*
and provides functions for working with the data of your study,
including visualization of results on brain meshes.

## Supported formats

You do **not** need to have FreeSurfer installed to use this package. It
implements its own readers and writers for the following file formats:

- MGH/MGZ: FreeSurfer 4-dimensional brain images or arbitrary other
  data. Typically a single 3D brain MRI scan or a time series of scans,
  or morphometry data for brain surfaces. The format is named after the
  Massachusetts General Hospital, and the specs are given (rather
  implicitely) [here in the FreeSurfer
  wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat).
  MGZ is just a gzipped version of MGH. An example file from the
  *recon-all* output for a subject would be `mri/T1.mgz` (containing a
  3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh`
  (containing surface data mapped to standard space). This format can be
  read and written. Reading and writing header data is also supported,
  and transforms like the ras2vox matrix can be computed from the
  header, allowing for the proper orientation of the voxel data in
  different spaces.

- FreeSurfer *curv* format: Morphometry data for a brain surface, one
  scalar per vertex. Could be the thickness or area of the cerebral
  cortex at each mesh vertex. Two versions of this format exist, an
  ASCII version and a binary version (the only one that is used in
  current FreeSurfer versions). An example file would be `surf/lh.area`.
  Both versions of this format can be read and written.

- FreeSurfer annotation file format: Contains a cortical parcellation. A
  cortical parcellation originates from a brain atlas and contains a
  label for each vertex of a surface that assigns this vertex to one of
  a set of atlas regions. (Put another way, a parcellation splits the
  brain surface into disjunct atlas regions). The file format also
  contains a colortable, which assigns a color code to each atlas
  region. An example file would be `labels/lh.aparc.annot`. This format
  can be read and written. The standard atlases that come with
  FreeSurfer are Desikan-Killiany (`aparc`), DKT (`aparc.DKTatlas40`),
  and Destrieux (`aparc.a2009s`).

- FreeSurfer surface file format: Contains a brain surface mesh in a
  binary format. Such a mesh is defined by a list of vertices (each
  vertex is given by its x,y,z coords) and a list of faces (each face is
  given by three vertex indices). An example file would be
  `surf/lh.white`. This format can be read and written. Reading and
  writing the ASCII version of the FreeSurfer surface format (`.asc`
  files) is also supported.

- Other mesh file formats: Read and write support is available for
  meshes in VTK legacy format (`.vtk` files, in the ASCII and the binary
  encoding, and in both cell array layouts that VTK versions write),
  Surf-Ice format (`.mz3`), Wavefront object format (`.obj`), Object
  File Format (`.off`), Brainvoyager SRF format (`.srf`), Stanford
  triangle format (`.ply`), and STL format (`.stl`, both the binary and
  the ASCII variant, which some tools write with the extensions `.stlb`
  and `.stla`). Additionally, meshes can be exported in PLY2 format
  (`.ply2`). Meshes can be imported from files in BYU format (`.byu`),
  GEO format (`.geo`) and TRI format (also known as ICO mesh format,
  `.tri`). The STL format is the format of 3D printing and of most mesh
  processing software, so a surface that was read by this package can be
  handed to those tools, and meshes written by them can be read back,
  even though the format stores the mesh as a polygon soup without a
  vertex list.

- FreeSurfer label file format: Contains a list of vertices included in
  a label. A label is like a mask, and is typically used to describe the
  vertices which are part of a certain brain region. An example file
  would be `label/lh.cortex.label`. Volume labels are also supported.
  This format can be read and written.

- FreeSurfer color lookup table (LUT) file format: Contains a color
  lookup table in ASCII format. This LUT assigns names and RGBA color
  values to a set of structures (typically brain regions). LUT data can
  also be extracted from an annotation, and a set of labels and a LUT
  can be merged into an annotation. An example file would be
  `FREESURFER_HOME/FreeSurferColorLUT.txt`. This format can be read and
  written. A brain atlas that is distributed as such a LUT plus a
  per-vertex label file in CSV format (one atlas region index per
  surface vertex) can be read with
  [`atlas.from.lut.and.csv()`](https://dfsp-spirit.github.io/freesurferformats/reference/atlas.from.lut.and.csv.md)
  and written back with
  [`write.atlas.to.lut.and.csv()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.atlas.to.lut.and.csv.md).
  This is how several cortical atlases are distributed by third-party
  tools (Desikan-Killiany, Brainnetome, Schaefer, …).

- FreeSurfer *weight* file format: Contains one value per listed vertex.
  In contrast to curv files, weight files contain values not for all
  vertices of a surface, but only for a subset of vertices defined by
  their indices. The format is known as *weight* format, *paint* format,
  or simply *w* format. This format can be read and written.

- FreeSurfer *patch* file format: Contains a subset of a surface (a
  *surface patch*), given by the vertex indices (and the faces in the
  ASCII version). For each patch vertex, it also stores whether the
  vertex is part of the patch border. This format can be read and
  written.

- Spatial transformation matrices: the FreeSurfer formats LTA,
  register.dat (tkregister) and xfm, the FSL matrix format written by
  FLIRT (`-omat`), and the ITK text transform format that 3D Slicer,
  ANTs, SimpleITK and the derivatives of fMRIPrep/QSIPrep work with can
  be read *and written*. Transformations are returned as instances of
  the `fs.transform` class, which state what the matrix means: which
  coordinates it maps between (voxel indices, RAS or LPS world
  coordinates) and which volumes it relates, so a matrix that operates
  on voxel indices cannot be mistaken for one that operates on world
  coordinates.
  [`transform2world()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2world.md),
  [`transform2voxel()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2voxel.md),
  [`transform2ras()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2ras.md)
  and
  [`transform2lps()`](https://dfsp-spirit.github.io/freesurferformats/reference/transform2lps.md)
  convert a transformation between these spaces (a volume or its header
  is passed where geometry is needed),
  [`invert.fs.transform()`](https://dfsp-spirit.github.io/freesurferformats/reference/invert.fs.transform.md)
  computes the reverse mapping, and
  [`summary()`](https://rdrr.io/r/base/summary.html) reports the
  properties in machine-readable form. A transformation that a file
  format cannot represent is not written, instead of being written in a
  form that silently means something else.

- FreeSurfer Group Descriptor (FSGD) files: please see the [fsbrain
  package](https://github.com/dfsp-spirit/fsbrain) for FSGD read and
  write file support. This is very handy if you conducted GLM-based
  statistical analyses in FreeSurfer and want to visualize the results
  in R.

- NIFTI v1: FreeSurfer morphometry data in NIFTI v1 format (including
  `.nii` and `.nii.gz`) files can be read and written with our own NIFTI
  v1 support. FreeSurfer NIFTI v1 files that use the (non-standard)
  FreeSurfer NIFTI hack are also supported. These files are created by
  FreeSurfer tools if NIFTI output is requested and one dimension of the
  data is larger than the 32k entries allowed by the NIFTI v1 standard.
  This affects virtually all surface-based data files because the brain
  surface meshes in FreeSurfer typically have more than 100k vertices.

- NIFTI v2: This package comes with its own NIFTI v2 reader and writer.
  The 2nd format version supports larger data dimensions and drops
  backwards compatibility with older NIFTI-style file formats like
  ANALYZE. Header extensions, the mechanism that the format provides for
  arbitrary additional data between the header and the voxel data (and
  that CIFTI-2 uses to store its XML metadata), can be read and written
  as well:
  [`nifti2.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.extension.md)
  creates one,
  [`write.nifti2()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.nifti2.md)
  writes it, and
  [`read.nifti2.header()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.nifti2.header.md)
  returns the extensions it found in the `extensions` field
  ([`nifti2.get.extension()`](https://dfsp-spirit.github.io/freesurferformats/reference/nifti2.get.extension.md)
  looks one up by its code).

- ANALYZE 7.5 (`.hdr` plus `.img`) and NIFTI v1 pair files: the two-file
  image formats can be read and written with our own implementation. The
  two variants share the 348 byte header but not its field semantics, so
  the package reads each of them the way its own specification defines
  it, and detects which one a file is from the header (the `smin` field
  of ANALYZE is the magic field of NIFTI, where `ni1` means a pair and
  an empty value means ANALYZE). This covers the volumes written by SPM,
  older scanners and 3D Slicer, and the `.hdr`/`.img` files of FSL, and
  it means that this package can also read back the NIFTI v1 files it
  writes. Note the fundamental limitation of ANALYZE 7.5: the format
  stores voxel sizes but no orientation, so the left/right direction of
  such an image is undefined. The transformation matrix that SPM and
  FreeSurfer store in the MATLAB sidecar file next to the image is read
  and used (that is the only reliable geometry such a file can have);
  for files without it, the readers do not invent a matrix (a wrong
  matrix silently mirrors a brain), and the voxel sizes and the
  orientation code are returned instead. The SPM/FreeSurfer
  interpretation of the header (their scale factor and image origin) can
  be requested explicitly. See
  [`read.fs.volume.analyze()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.analyze.md)
  and
  [`write.analyze()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.analyze.md).

- NRRD volumes (`.nrrd` and `.nhdr`, “nearly raw raster data”): read
  support, with no dependency on other packages. This is the volume
  format of 3D Slicer, the ITK/VTK tools and several diffusion MRI
  pipelines, and the header can carry diffusion information (b-value,
  gradient directions, measurement frame) or arbitrary key-value
  metadata. All data types, all four encodings (`raw`, `ascii`, `gzip`,
  `bzip2`), gzip-compressed whole files, detached headers whose data
  lives in another file (optionally compressed), a list of several data
  files, and the `line skip`/`byte skip` fields are supported. The world
  transformation is returned as the same `vox2ras_matrix` that the NIFTI
  and MGH readers produce (including the conversion from the
  left-posterior-superior convention of NRRD), and diffusion metadata is
  returned as a `dwi` header property, so such a volume can be used just
  like an MGH/MGZ volume. Reading is verified value by value against
  `pynrrd` and the geometry against ITK (see
  `dev_tools/check_nrrd_conversion.R`). Writing NRRD is not supported.

- Fiber track formats (DTI, diffusion tensor imaging): the ‘.trk’ format
  used by [Diffusion Toolkit / TrackVis](http://www.trackvis.org/dtk/)
  and the ‘.tck’ and ‘.tsf’ formats used by
  [MRtrix3](https://www.mrtrix.org/) can all be read and written, both
  uncompressed and gzip-compressed (`.tck.gz`, `.trk.gz`, `.tsf.gz`).
  The compression is detected from the file content rather than from the
  file name, so a renamed file is read correctly, and a compressed file
  can be produced directly by the writers (TrackVis and MRtrix do not
  read compressed track files themselves, so the compression is meant
  for archiving and for passing files between the programs of this
  project). A TSF file stores one value per point along a track
  (e.g. the fractional anisotropy, the distance along the track, or
  values sampled from an image at the point coordinates) and is always
  used together with the tractogram it describes, since it stores no
  track boundaries. Streamlines that other software exported as a VTK
  polydata file with a `LINES` section (as Paraview, TrackVis and DSI
  Studio do) can be read with
  [`read.fs.tracts.vtk()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.tracts.vtk.md).

- Large tractograms: a whole-brain tractogram is a multi-GB file with
  millions of streamlines, so the track readers are built for streaming.
  They can be asked for a subset via `max_tracks`, can skip over tracks
  (`skip_tracks`) and can filter by a bounding box while reading (tracks
  that are filtered out are never held in memory), so a region of
  interest can be extracted from a huge file. There are also functions
  to count the tracks
  ([`dti.track.count()`](https://dfsp-spirit.github.io/freesurferformats/reference/dti.track.count.md)),
  to compute their bounding box
  ([`dti.track.bbox()`](https://dfsp-spirit.github.io/freesurferformats/reference/dti.track.bbox.md))
  and to iterate over them one at a time with a constant memory
  footprint
  ([`dti.track.iterator()`](https://dfsp-spirit.github.io/freesurferformats/reference/dti.track.iterator.md)),
  and the file headers can be read on their own without touching the
  track data. Track coordinates are returned in an `fs.tracts`
  container, which keeps the coordinates of all tracks in a single
  matrix plus the number of points per track, so that a tractogram needs
  far less memory than a list with one matrix per track; `tracks[[i]]`,
  [`length()`](https://rdrr.io/r/base/length.html) and
  [`lapply()`](https://rdrr.io/r/base/lapply.html) work as for a list,
  and [`as.list()`](https://rdrr.io/r/base/list.html) converts to the
  legacy list of matrices.

- Diffusion MRI gradient tables (b-vectors and b-values): the FSL
  format, i.e. a ‘.bvec’ and ‘.bval’ file pair, and the MRtrix gradient
  table format can be read and written. Both the layout written by the
  FSL tools (three lines of vector components, all b-values in one line)
  and the layout used by other tools (one volume per line, as
  distributed by the Human Connectome Project) are detected
  automatically. Reading b-vectors and b-values together verifies that
  they match, and reports questionable entries – missing values,
  gradient vectors that are not unit vectors, or a b-value without a
  direction – instead of silently changing them. Note that the gradient
  vectors in these files refer to the *image* axes, so they are only
  meaningful together with the image they belong to.

We also provide wrappers and adapter functions for existing neuroimaging
file format packages, which load the data into *freesurferformats* data
structures:

- NIFTI volumes (v1, single file): Reading is supported based on the
  [oro.nifti](https://CRAN.R-project.org/package=oro.nifti) package. The
  result is transformed into an `fs.volume` instance, including
  computation of transformation matrices like vox2ras from the NIFTI
  header q-form/s-form, so NIFTI volumes can be used just like MGH/MGZ
  volumes. (Note: If you do not need the FreeSurfer-style transforms and
  all you want is to read NIFTI files, you should use `oro.nifti`
  directly.) Alternatively, our internal NIFTI1 reader can be used,
  which also supports non-standard FreeSurfer NIFTI1 files (see above).

- GIFTI: General reading is supported based on the
  [gifti](https://CRAN.R-project.org/package=gifti) and
  [xml2](https://CRAN.R-project.org/package=xml2) packages. GIFTI is a
  very versatile format that can hold different kinds of data, and
  *freesurferformats* provides custom readers for morphometry data,
  surface meshes, labels and annotations. The *freesurferformats* also
  comes with GIFTI write support, including a general data array writer
  as well as custom writers for the previously listed kinds of
  neuroimaging data.

- CIFTI: Reading of CIFTI v2 files (the NIFTI v2 based format used by
  the Human Connectome Project) is native, i.e. it does not depend on
  any other R package.
  [`read.cifti.header()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.header.md)
  parses the XML metadata of a file into an `fs.cifti` object that
  describes the matrix dimensions (series, brainordinates, parcels,
  scalars or labels), the brain models with their surface vertex and
  volume voxel indices, the parcels, the label tables and the series
  information,
  [`read.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.md)
  additionally reads the data matrix, and
  [`cifti.structures()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structures.md),
  [`cifti.parcels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.parcels.md),
  [`cifti.grayordinates()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.grayordinates.md),
  [`cifti.series.info()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.series.info.md),
  [`cifti.label.table()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.label.table.md)
  and
  [`cifti.dim.labels()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.dim.labels.md)
  extract the common parts. The readers
  [`read.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.cifti.md),
  [`read.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.parcellation.cifti.md)
  and
  [`read.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.series.cifti.md)
  reconstruct the data for a single brain surface (the missing medial
  wall vertices of a grayordinates file become `NA`),
  [`cifti.structure.data()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.structure.data.md)
  does the same for any structure, including volume structures, for
  which it returns the voxel indices and the affine transformation
  instead of pretending to have per-vertex data, and
  [`read.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.connectome.cifti.md)
  reads the connectome types (`.dconn`, `.pconn` and the mixed
  `.pdconn`/`.dpconn`, optionally only selected rows and columns, which
  is the only way to work with the multi-GB `.dconn` of a real subject).
  [`read.cifti.rows()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.cifti.rows.md)
  reads selected rows of such a file without loading the whole matrix
  (e.g. the first time points of a `.dtseries`), and selecting *columns*
  reads only the requested columns of the file. All nine standard
  CIFTI-2 file types are supported, for surface, volume and mixed brain
  models. Writing is native as well:
  [`write.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.cifti.md)
  writes any of the nine file types from the axes that the
  `cifti.axis.*()` builders create (or from the mapping of a template
  file, which is what real data needs),
  [`write.fs.morph.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.morph.cifti.md),
  [`write.fs.series.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.series.cifti.md)
  and
  [`write.fs.parcellation.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellation.cifti.md)
  write the data representations this package uses for surfaces,
  [`write.fs.connectome.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.connectome.cifti.md)
  and
  [`write.fs.parcellated.cifti()`](https://dfsp-spirit.github.io/freesurferformats/reference/write.fs.parcellated.cifti.md)
  write connectomes and parcellated maps or series,
  [`cifti.axis.parcels.from.annot()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.axis.parcels.from.annot.md)
  builds the parcels of a parcellated file from the annotations of an
  atlas, and
  [`cifti.header.from.axes()`](https://dfsp-spirit.github.io/freesurferformats/reference/cifti.header.from.axes.md)
  builds the CIFTI-2 XML for inspection. Note that CIFTI files must not
  be gzipped (the format forbids it), so `.dscalar.nii.gz` is an error.
  The reading and writing code is verified against nibabel and
  Connectome Workbench, see `dev_tools/check_cifti_conversion.R`. Note
  that a CIFTI file is a NIFTI file, so the generic readers
  [`read.fs.morph()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.morph.md)
  and
  [`read.fs.volume()`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.volume.md)
  would match it: they detect CIFTI files and point you at the CIFTI
  readers instead of returning the values of the matrix as a volume or a
  per-vertex vector.

## News

- 2026-09-08: New freesurferformats version v1.0.2 released on CRAN, see
  the
  [CHANGES](https://dfsp-spirit.github.io/freesurferformats/CHANGES).
- 2026-07-08: New freesurferformats version v1.0.1 released on CRAN, see
  the
  [CHANGES](https://dfsp-spirit.github.io/freesurferformats/CHANGES).
- 2025-09-09: New freesurferformats version v1.0.0 released on CRAN, see
  the
  [CHANGES](https://dfsp-spirit.github.io/freesurferformats/CHANGES).
- 2024-02-03: New freesurferformats version v0.1.18 released on CRAN,
  see the
  [CHANGES](https://dfsp-spirit.github.io/freesurferformats/CHANGES).
- 2022-02-12: New freesurferformats version v0.1.17 released on CRAN,
  see the
  [CHANGES](https://dfsp-spirit.github.io/freesurferformats/CHANGES).

You can also find all releases in the [releases
section](https://github.com/dfsp-spirit/freesurferformats/releases).

## Installation

The package is on
[CRAN](https://CRAN.R-project.org/package=freesurferformats), so you can
simply:

``` r

install.packages("freesurferformats")
```

If you want support for as many neuroimaging file formats as possible,
do this instead to install with all optional dependencies:

``` r

install.packages("freesurferformats", dependencies=TRUE);
```

In case something goes wrong, don’t worry. Just install the missing
[system dependencies](#system-dependencies) and retry.

### System dependencies

*Note:* You can ignore this section unless you want to build the
freesurferformats package from the source code.

A *system dependency* is a **non-R** software that is needed for the
installation of a package. System dependencies cannot be installed
automatically using the R package system, so you need to install them
manually or using the package manager of your operating system.

If you install R packages from source (the default under Linux) and want
support for the GIFTI XML file format, you will need `libxml2-dev`. If
you do not have it installed already, before installing
*freesurferformats*, run the following command in your system shell (not
in R):

- for deb-based Linux distributions (Debian, Ubuntu, …):

``` shell
sudo apt-get install libxml2-dev libcurl4-openssl-dev
```

- for rpm-based Linux distributions (Fedora, CentOS, RHEL, …):

``` shell
sudo yum install libxml2-devel libcurl-devel
```

## Documentation

### Quick Usage

Before using any functions, of course load the package itself:

``` r

library("freesurferformats")
```

Now you can call the following functions:

``` r

read.fs.mgh()         # read volume or morphometry data from files in MGH or MGZ format, e.g., `mri/brain.mgz` or `surf/lh.area.fwhm10.fsaverage.mgh`.
read.fs.volume.nrrd() # read a volume in NRRD format (`.nrrd`/`.nhdr`), e.g. one written by 3D Slicer.
read.fs.volume.analyze() # read a volume in ANALYZE 7.5 or two-file NIFTI v1 format (`.hdr`/`.img`), e.g. FSL output.
read.fs.curv()        # read morphometry data from 'curv' format files like `surf/lh.area`
read.fs.morph()       # read any morphometry file (mgh/mgz/curv). The format is derived from the file extension.
read.fs.annot()       # read annotation data or brain atlas labels from files like `label/lh.aparc.annot`
read.fs.surface()     # read a surface mesh, like `surf/lh.white`, supports many standard mesh formats
read.fs.label()       # read a label file, like `label/lh.cortex.label`
read.fs.colortable()  # read a color lookup table (LUT), like `$FREESURFER_HOME/FreeSurferColorLUT.txt`
read.fs.weight()      # read scalar data for a subset of vertices, defined by index. Known as `weight`, `paint` or simply `w` format.
read.fs.patch()       # read a surface patch, which is a part of a surface.
read.fs.transform()   # read spatial transformation matrix
read.dti.tck()        # read DTI tracks from MRtrix3 'TCK' format
read.dti.trk()        # read DTI tracks from Diffusion Toolkit/TrakVis 'TRK' format
read.dti.tsf()        # read DTI per-point track scalar data from MRtrix3 'TSF' format
read.dti.gradients()  # read and validate a diffusion gradient table (FSL '.bvec'/'.bval' pair or MRtrix format)
read.fs.tracts.vtk()  # read streamlines from a VTK polydata file, as written by Paraview, TrackVis or DSI Studio
read.fs.parcellation.cifti() # read a cortical parcellation from a CIFTI dlabel file
read.cifti()          # read the data matrix and the metadata of a CIFTI-2 file
read.cifti.rows()     # read selected rows of a large CIFTI-2 file without loading it
read.cifti.header()   # read only the XML metadata of a CIFTI-2 file
cifti.structure.data() # extract the data of one brain structure (surface or volume) from a CIFTI-2 file
write.cifti()         # write a CIFTI-2 file (any of the nine standard file types)
write.fs.morph.cifti() # write morphometry data to a CIFTI dscalar file
write.fs.series.cifti() # write time series data to a CIFTI dtseries file
write.fs.parcellation.cifti() # write a parcellation to a CIFTI dlabel file
read.fs.connectome.cifti() # read a CIFTI connectome (dconn, pconn, pdconn, dpconn)
write.fs.connectome.cifti() # write a CIFTI connectome (dconn, pconn, pdconn, dpconn)
write.fs.parcellated.cifti() # write a parcellated CIFTI file (pscalar, ptseries)
cifti.axis.parcels.from.annot() # build the parcels of a parcellated CIFTI file from an atlas

write.fs.mgh()        # write data with 1 to 4 dimensions to an MGH format file
write.fs.curv()       # write a data vector to a 'curv' format file
write.fs.morph()      # write any morphometry file (mgh/mgz/curv). The format is derived from the file extension.
write.fs.surface()    # write a surface mesh
write.fs.label()      # write a label file
write.fs.annot()      # write an annotation file
write.fs.colortable() # write a color lookup table (LUT)
write.fs.weight()     # write scalar vertex data in weight or w format
write.fs.patch()      # write a surface patch, which is a part of a surface.
write.fs.transform()  # write a spatial transformation matrix (LTA, register.dat, xfm, FSL or ITK format)
write.dti.tck()       # write DTI tracks to MRtrix3 'TCK' format
write.dti.trk()       # write DTI tracks to Diffusion Toolkit/TrackVis 'TRK' format
write.dti.tsf()       # write DTI per-point track scalar data to MRtrix3 'TSF' format
write.dti.bvec()      # write b-vectors in the FSL '.bvec' format (see also write.dti.bval(), write.dti.grad())
write.atlas.to.lut.and.csv() # write a brain atlas to a colortable (LUT) and a per-vertex label file
```

The documentation is included in the package and not repeated on this
website.

### Full Documentation

The documentation can be accessed from within an R session after you
have loaded the *freesurferformats* package:

- Detailed vignettes with explanations and examples for the usage of all
  functions of the package are included, run
  `browseVignettes("freesurferformats")` to see them. You can also open
  the vignettes directly:
  - learn how to read neuroimaging data:
    [`vignette("freesurferformats")`](https://dfsp-spirit.github.io/freesurferformats/articles/freesurferformats.md)
    [read online at
    CRAN](https://cran.r-project.org/web/packages/freesurferformats/vignettes/freesurferformats.html)
  - learn how to write neuroimaging data:
    [`vignette("freesurferformats_write")`](https://dfsp-spirit.github.io/freesurferformats/articles/freesurferformats_write.md)
    [read online at
    CRAN](https://cran.r-project.org/web/packages/freesurferformats/vignettes/freesurferformats_write.html)
  - learn advanced header-based operations:
    [`vignette("freesurferformats_header")`](https://dfsp-spirit.github.io/freesurferformats/articles/freesurferformats_header.md)
    [read online at
    CRAN](https://cran.r-project.org/web/packages/freesurferformats/vignettes/freesurferformats_header.html)
- Help for a specific function can be accessed in the usual R manner:
  `?<function>`, where you replace `<function>` with a function name.
  Like this:
  [`?read.fs.mgh`](https://dfsp-spirit.github.io/freesurferformats/reference/read.fs.mgh.md).
- Run `example(<function>)` to see a live demo that uses the function
  `<function>`. Like this: `example(read.fs.mgh)`.
- The [unit
  tests](https://dfsp-spirit.github.io/freesurferformats/tests/testthat/)
  that come with this package are essentially a list of examples that
  illustrate how to use the functions.

### An example R session: Reading Bert’s brain

One of the example subjects that comes with FreeSurfer is `bert`. The
following example shows how to load Bert’s brain. If you have FreeSurfer
installed, you can start GNU R by typing `R` in your favourite terminal
application and run the following commands:

``` r

install.packages("freesurferformats")

# Load volume from file
library("freesurferformats")
berts_brain = paste(Sys.getenv("FREESURFER_HOME"), "/subjects/bert/mri/brain.mgz", sep="")
mgh = read.fs.mgh(berts_brain, with_header=TRUE);

# Compute the vox2ras matrix from the header:
mghheader.vox2ras(mgh)
#     [,1] [,2] [,3]      [,4]
#[1,]   -1    0    0  133.3997
#[2,]    0    0    1 -110.0000
#[3,]    0   -1    0  128.0000
#[4,]    0    0    0    1.0000

# ...and the data:
mean(mgh$data)
#[1] 8.214322
dim(drop(mgh$data))
# [1] 256 256 256
```

If you do not have FreeSurfer installed and thus don’t have Bert,
replace `berts_brain` with the example brain that comes with
freesurferformats:

``` r

fsf_brain = system.file("extdata", "brain.mgz", package = "freesurferformats", mustWork = TRUE);
```

## Security configuration

The freesurferformats package includes protection against malformed or
malicious input files, including a maximum allocation limit that
defaults to 2 GB of memory per file. This should be plenty for standard
neuroimaging applications, and you should typically not need to worry
about this. However, if you are working with exceptionally large files,
or in environments where you allow users to upload files you work with
and expect very small files, it is definitely worth it to adapt the
limits. Here is how to do that:

``` r

# Directly in an R session or script, via R options (recommended for interactive use):
options(freesurferformats.max_alloc_bytes = 1e9)   # 1 GB
options(freesurferformats.max_alloc_bytes = Inf)    # disable (use with care)
```

``` shell
# Via environment variable (recommended for CI/containers):
Sys.setenv(FREESURFERFORMATS_MAX_ALLOC_BYTES = 5e8) # 500 MB
```

## License and Author

Written by [Tim Schäfer](https://ts.rcmd.org).

The *freesurferformats* package is [free
software](https://en.wikipedia.org/wiki/Free_software), published under
the [MIT license](https://opensource.org/licenses/MIT).

Note: The file LICENSE in this repository is a CRAN license template
only (as required by CRAN) and does not contain the full MIT license
text. See the file
[LICENSE_FULL](https://dfsp-spirit.github.io/freesurferformats/LICENSE_FULL)
for the full license text.

## Citation

A paper is in the making. For now, please cite the R package. You can
generate the citation for the version you use by typing the following
command in R:

    citation("freesurferformats")

This will ouput something like this (but for the version you actually
used, which is important for reproducibility):

    To cite package ‘freesurferformats’ in publications use:

      Tim Schäfer (2020). freesurferformats: Read and Write 'FreeSurfer'
      Neuroimaging File Formats. R package version 0.1.9.
      https://CRAN.R-project.org/package=freesurferformats

    A BibTeX entry for LaTeX users is

      @Manual{,
        title = {freesurferformats: Read and Write 'FreeSurfer' Neuroimaging File Formats},
        author = {Tim Schäfer},
        year = {2020},
        note = {R package version 0.1.9,
        url = {https://CRAN.R-project.org/package=freesurferformats},
        doi = {10.5281/zenodo.3540434},
        url = {https://dx.doi.org/10.5281/zenodo.3540434},
      }

The Digital Object Identifier (DOI) for *freesurferformats* is:
[10.5281/zenodo.3540434](https://dx.doi.org/10.5281/zenodo.3540434).
Note that this DOI always points to the latest version, so be sure to
still include the package version in the citation.

A poster of *freesurferformats* has been presented at INSAR 2020 Annual
Meeting:
[Abstract](https://insar.confex.com/insar/2020/meetingapp.cgi/Paper/33181),
[ePoster](https://insar.confex.com/insar/2020/techdemo/eposter.cgi?eposterid=227)

## Developer Information

Please refer to
[README_DEVELOPMENT.md](https://dfsp-spirit.github.io/freesurferformats/README_DEVELOPMENT.md).
