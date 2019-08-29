# freesurferformats
GNU R package to read FreeSurfer neuroimaging file formats.

## Supported formats

* MGH/MGZ: FreeSurfer n-dimensional brain images or arbitrary other data. Ttypically a single 3D brain MRI scan or a time series of scans, or morphometry data for brain surfaces. The format is named after the Massachusetts General Hospital, and the specs are given (rather implicitely) [here in the FreeSurfer wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat). An example file would be `mri/T1.mgz` (containing a 3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh` (containing surface data mapped to standard space).

* FreeSurfer 'curv' format: Morphometry data for a brain surface, one scalar per vertex. Could be the thickness or area of the cerebral cortex at each mesh vertex. Several versions of this format exist, the supported version is the new, binary one (the only one that is used in current FS versions). An example file would be `surf/lh.area`.

## Functions

* reaf.fs.mgh -- Read FreeSurfer MGH or MGZ format file
* read.fs.curv -- Read FreeSurfer curv format file


## License

MIT (See LICENSE file)
