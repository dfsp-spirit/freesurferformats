# freesurferformats
GNU R package to read FreeSurfer neuroimaging file formats.

## Supported formats

* MGH/MGZ: FreeSurfer n-dimensional brain images or arbitrary other data. Ttypically a single 3D brain MRI scan or a time series of scans, or morphometry data for brain surfaces. The format is named after the Massachusetts General Hospital, and the specs are given (rather implicitely) [here in the FreeSurfer wiki](https://surfer.nmr.mgh.harvard.edu/fswiki/FsTutorial/MghFormat). An example file would be `mri/T1.mgz` (containing a 3D brain volume), but also `surf/lh.area.fwhm15.fsaverage.mgh` (containing surface data mapped to standard space).

* FreeSurfer 'curv' format: Morphometry data for a brain surface, one scalar per vertex. Could be the thickness or area of the cerebral cortex at each mesh vertex. Several versions of this format exist, the supported version is the new, binary one (the only one that is used in current FS versions). An example file would be `surf/lh.area`.

## Functions

* reaf.fs.mgh -- Read FreeSurfer MGH or MGZ format file

    ```brain_3D_voxels = read.fs.mgh(system.file("mystudy", "subject1", "mri", "brain.mgz"));```
    
* read.fs.curv -- Read FreeSurfer curv format file
    
    ```cortical_thickness = read.fs.curv(system.file("mystudy", "subject1", "surf", "lh.thickness"));```

## Documentation

* A short vignette/howto for the package is included, run `browseVignettes("freesurferformats")` to see it.
* Help for a specific function can be accessed in the usual R manner: `?function`, where you replace `function` with something like `reaf.fs.mgh`.
* Run `example(function)` to see a live demo that uses the function `function`.


## License

MIT (See [LICENSE](./LICENSE) file)
