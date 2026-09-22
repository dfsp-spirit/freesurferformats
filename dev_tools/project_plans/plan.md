# File format expansion plan

Goal: grow *freesurferformats* beyond its FreeSurfer origin so that it is also a
practical low-level I/O package for the **fMRI/HCP** and **DWI (diffusion MRI)**
communities, without breaking the existing FreeSurfer/CAT12/FSL/BrainVoyager
support.

Status legend: `[ ]` todo, `[~]` in progress, `[x]` done, `[!]` blocked, `[-]` dropped.

---

## 0. Baseline: what the package supports today (2026-09-17)

Own readers + writers (no FreeSurfer installation required):

| Domain | Formats |
| --- | --- |
| Volumes | MGH/MGZ (incl. header, `vox2ras`, RAS<->vox), NIfTI-1 (`.nii`, `.nii.gz`, FS fsnifti hack), NIfTI-2, ANALYZE 7.5 + NIfTI-1 pairs (`.hdr`/`.img`, R/W), NRRD (`.nrrd`/`.nhdr`, read-only: all types + encodings, detached/LIST, DWI metadata, ITK-verified geometry) |
| FS morphometry | `curv` (binary + ASCII), `weight`/`w`/paint, `patch` (binary + ASCII), morph in `mgh`/`mgz`/`gii`/`nii`/`ni1`/`ni2`/`txt`/`asc`/`smp` |
| FS labels/atlases | `annot`, label (surface + volume), colortable LUT, LUT+CSV atlas |
| Surfaces | FS binary surface, `.asc`, GIFTI, MZ3, OBJ, OFF, PLY, PLY2, VTK (ASCII + binary, both VTK cell array layouts), SRF, STL (ASCII + binary, R/W); read-only: BYU, GEO, TRI/ICO |
| BrainVoyager | SMP (R/W), SRF |
| Tracts | TRK (R/W, incl. `.trk.gz`), TCK (R/W), TSF (R/W) + headers, streaming, `bbox`/`skip_tracks`, `fs.tracts` |
| Transforms | LTA, `register.dat`, `xfm`, FSL `-omat`, ITK text (`.tfm`/`.txt`) -- all R/W, `fs.transform` class |
| GIFTI | morph, surface, label, annot (R/W), generic data array writer |
| CIFTI-2 | `.dscalar.nii`, `.dlabel.nii`, `.dtseries.nii` -- **read only**, via the `cifti` package |

Comparison baseline: nibabel supports NIfTI1/2, ANALYZE (plain/SPM99/SPM2),
GIFTI, CIFTI-2 (full R/W), FreeSurfer (MGH/annot/label/morph/geometry), MINC1,
MINC2, AFNI BRIK/HEAD (read-only), ECAT7, Philips PAR/REC, DICOM (`nicom`,
read-only), and streamlines (TRK, TCK). Verified against a local checkout of
nibabel 5.4.2-131-g699b9923 at `~/builds_and_patches/nibabel`. Two facts that
were worth checking and that shape the plan: nibabel has **no** reader for a
`.bvec`/`.bval` file pair (gradients only appear in `parrec.get_bvals_bvecs()`,
for Philips PAR/REC headers), and `nibabel/streamlines/` contains **no TRX
module** -- so neither of the two Tier 1 DWI items is a port of existing
nibabel code, and DIPY (for gradients) and MRtrix3 (for `.grad`) are the
references to match.

Where freesurferformats already leads: OBJ/OFF/PLY/STL/MZ3/VTK/SRF/BYU/GEO
meshes, TSF, weight/paint/patch/colortable, `register.dat`/LTA/xfm,
BrainVoyager formats, multi-format `write.fs.morph()` dispatch.

Nibabel's structural advantage for the fMRI crowd is not its format list but
lazy, memory-mapped array access (`ArrayProxy`) -- see item **V.1**.

---

Locally available test data (chose small files from it and copy into repo into extra_test_data/ dir, not into inst/extdata due to CRAN 5 MB limit. in tests we will need to check whether data is there and skip tests if not)

Locally there is a subject, raw DICOMs and preprocessing with various standard tools, in ~/develop/sub-01/ (raw DICOMS, bids) and the rest in ~/develop/sub-01-derived/. check this for test data.

---

## I. Tier 1 -- highest value for fMRI/HCP + DWI

### I.1 [~] Diffusion gradient tables: FSL `.bvec`/`.bval` + MRtrix `.grad`

Implemented in `R/read_dwi_gradients.R` and `R/write_dwi_gradients.R`. The
`read.fsl.*` / `read.mrtrix.*` names from the original sketch were dropped: the
package's existing DWI namespace is `dti.*` (`read.dti.tck`, `read.dti.trk`).

- [x] Return layout decided: `bvec` is an N x 3 matrix with columns x, y, z and
      one row per volume; `bval` is a numeric vector of length N.
- [x] `read.dti.bvec()` / `write.dti.bvec()`, incl. 3xN vs Nx3 detection
- [x] `read.dti.bval()` / `write.dti.bval()` (single line and one value per line)
- [x] `read.dti.grad()` / `write.dti.grad()` (MRtrix format, columns x, y, z, b)
- [x] `read.dti.gradients()` entry point, with automatic b-values file lookup
- [x] `validate.dti.gradients()` (internal): length and `n_volumes` checks, the
      NA/NaN rules, and reporting of non-unit norms / zero directions
- [x] `layout` parameter ('auto' / 'components' / 'volumes') to override detection
- [x] Tests, including a real QSIPrep pair in `extra_test_data/dwi/`
- [ ] Voxel <-> scanner conversion (`fsl2mrtrix`-style). Needs the *rotation*
      part of the image transform, i.e. the direction cosines with the voxel
      sizes divided out, plus MRtrix3's determinant-based first-axis flip.
- [ ] FSL sign/hemisphere convention: documented in the code comments, but the
      conversion itself is what makes it observable to users.
- Effort: S-M. See section "Gradients: detailed spec" below.

### I.2 [ ] CIFTI-2 write + read of `.dconn`/`.pconn`/`.ptseries`
- Binary layer is NIfTI-2 + an XML extension; the package already has an NIfTI-2
  writer and `xml2`, so a native writer is feasible without the `cifti` dep.
- The hard part is the index-map/brain-model bookkeeping (dense vertex indices
  vs parcels, `MEDIAL_WALL_ROI`), not the container.
- Reading is currently delegated to the `cifti` package (read-only, Soft dep);
  `.dconn` (dense connectomes, standard HCP resting-state output) cannot be read
  at all today.
- Effort: M-L.

### I.3 [ ] BIDS metadata sidecars
- `*_bold.json`, `*_dwi.json` (`RepetitionTime`, `SliceTiming`,
  `PhaseEncodingDirection`, `TotalReadoutTime`, `EchoTime`), `*_events.tsv`,
  `dataset_description.json`.
- `jsonlite`/`read.table` make this simple; this is what makes a format usable in
  a real HCP/BIDS dataset, and it pairs with I.1.
- Effort: S.

### I.4 [ ] FSL/ANTs/ITK transform matrices (`.mat`, `.tfm`, `.h5`)
- The package can read FS transforms but cannot **write any transform at all**.
- FSL `.mat` is a 4x4 text matrix (trivial); ITK `.tfm` is a small text format.
- Effort: S-M.

### I.5 [x] VTK legacy **binary** -- DONE
- Only VTK ASCII is supported. Binary VTK is the mesh/`POLYDATA` interchange for
  Paraview, TrackVis and DSI Studio streamline export.
- Extending the existing VTK reader/writer covers surfaces *and* streamlines.
- Effort: M.
- Done: the reader now supports both encodings and both cell array layouts (the
  `OFFSETS`/`CONNECTIVITY` layout of VTK 5.1+ and the counts+indices layout of
  VTK 4.2), detects the layout from the content, reads `LINES` sections as
  `fs.tracts` (`read.fs.tracts.vtk()`) and skips attribute sections; the writer
  got the `version` and `binary` parameters. A pre-existing bug was found on the
  way: the reader could not read *any* file written by VTK 5.1+ (wrong cell
  layout) and no VTK ASCII file with more than one coordinate per line, i.e. it
  failed on Paraview exports. Details and the verification against VTK,
  FreeSurfer and a file from the VTK 4.2 era are in `R/vtk_legacy.R`,
  `dev_tools/check_vtk_conversion.R` and `CHANGES` (Version 1.1.0).
- Not done: writing streamlines (`write.fs.tracts.vtk()`), which is a small
  follow-up now that the cell array encoder is in place; `TRIANGLE_STRIPS` and
  non-triangular polygons are rejected by name.

---

## II. Tier 2 -- strong value, more work

### II.1 [ ] TRX (`.trx`) streamlines, read + write
- Modern HCP-scale successor to TRK/TCK; per-streamline/per-point data,
  header-driven, memory-mappable. Supported by MRtrix3, DIPY, TrackVis and
  DSI Studio. Verified absent from nibabel 5.4.2+ (`nibabel/streamlines/` has no
  TRX module), so there is no Python implementation to copy from either.
- `fs.tracts` already uses the contiguous-matrix + lengths layout that TRX stores
  on disk (see comments in `R/trackvis_affine.R` and `R/fs_tracts.R`).
- Effort: L.

### II.2 [x] ANALYZE 7.5 `.hdr`/`.img`, read + write
- Same 348-byte header as NIfTI-1 with a different magic -> almost free, a variant
  of the existing NIfTI-1 reader. Also gives FSL `.img`/`.hdr` pairs.
- Best value per line of code on this list.
- Done (2026-09): read + write in `R/read_analyze.R` / `R/write_analyze.R`
  (`read.analyze.header()`, `read.analyze.data()`, `read.fs.volume.analyze()`,
  `write.analyze()`, `analyzeheader.template()`, `analyzeheader.for.data()`,
  `is.analyze.file()`), plus NIFTI-1 *pair* support in the NIFTI layer
  (`read.nifti1.data()` reads pairs, `write.nifti1()` writes them when the magic is
  `ni1`, `ni1header.template(pair=)`), dispatch in `read.fs.volume()` /
  `write.fs.volume()`, and gzipped pairs. Cross-validated against nibabel and
  FreeSurfer: `dev_tools/check_analyze_conversion.R` (22 checks, 0 failures),
  fixtures from `dev_tools/generate_analyze_test_data.py`. Two real bugs in the
  NIFTI layer were found on the way, see the progress log below.
- Effort: S-M (the format is small, the geometry conventions are the work).
- Done as well: the SPM/FreeSurfer `.mat` sidecar (item II.2b below), because it is the only *reliable* geometry
  an ANALYZE file can have.

### II.2b [x] Read the SPM/FS `.mat` sidecar of ANALYZE images (read-only)
- SPM and FreeSurfer write `<base>.mat` next to `<base>.img`: a MATLAB v4 file with
  a 4x4 double matrix `M`, the only *reliable* geometry of an ANALYZE file (nibabel
  reads it, `Spm99AnalyzeImage.from_file_map`).
- MATLAB v4 is uncompressed: 20 byte header + name + raw doubles, ~25 lines of R,
  no dependency. Detect it by the 'M'/'mat' name and validate the size.
- Would make `spm = TRUE` unnecessary for FS/SPM files, and would allow a faithful
  round trip of such files.
- Done (2026-09): `read.matlab.v4.matrix()` (internal, ~60 lines, all numeric
  types, several variables, both byte orders) plus `analyze.mat.sidecar.to.vox2ras()`,
  wired into `read.fs.volume.analyze()`: a sidecar that is present is used by
  default (`vox2ras_source = 'mat sidecar'`), and one that cannot be used is
  reported with a warning while the data is still read. Verified against nibabel
  for a nibabel-written Spm99 fixture and for FreeSurfer's own output, see
  `dev_tools/check_analyze_conversion.R` (24 checks, 0 failures).
- Effort: S (as estimated). Not done: writing a sidecar (the writers do not
  produce one; a NIFTI pair can store the geometry properly, which is better).
  Read-only, as planned.

### II.3 [ ] GIFTI multi-array / time-series write (`.func.gii`, `.dtseries.gii`)
- Reading is good, but the writer is morph-oriented. Surface fMRI in the
  HCP/workbench world uses multiple data arrays + time-axis metadata.
- Effort: M.

### II.4 [x] NRRD (`.nrrd`/`.nhdr`), read-only
- 4D-capable, header can carry DWI gradients and ROI metadata; used by 3D Slicer,
  DTI-TK and increasingly in dMRI tooling.
- ~~Not in nibabel's documented format list either (Python uses `pynrrd`), and
  **there is no NRRD support anywhere in R** -- an open niche.~~ **Partly wrong,
  corrected**: the nibabel part holds up (verified: `nibabel.load` fails on an
  NRRD file with `ImageFileError: Cannot work out file type`, Python uses
  `pynrrd`), but "no support in R" was wrong -- `nat` (CRAN) has
  `read.nrrd()`/`write.nrrd()` and `RIA` has a reader. Verified in the `nat` source
  (clone in `~/builds_and_patches/nat`): it handles detached `.nhdr` files and the
  `data file: LIST` mode, but only the encodings `raw`, `gzip` and `text` (= ascii)
  -- **no bzip2** -- it skips `measurement frame` as an unhandled field, ignores the
  `DWMRI_*` diffusion fields, and derives no voxel-to-world matrix from the space
  fields. So the niche is narrower than stated, but it exists (bzip2, DWI metadata,
  world matrix, ITK-verified reader) -- and the demand is modest: NRRD is not a
  FreeSurfer format, it is a Slicer/ITK/dMRI-pipeline format.
- Done (2026-09): read-only reader in `R/read_nrrd.R` (`read.nrrd.header()`,
  `read.fs.volume.nrrd()`, registered in `read.fs.volume()` for `.nrrd`/`.nhdr`).
  All 4 encodings incl. bzip2, all 10 data types, whole-file gzip, detached +
  compressed data files, `data file: LIST`, `line skip`/`byte skip` (incl. `-1`),
  `key:=value` syntax, LPS->RAS world matrix with shear preserved, and a `dwi`
  header property (b-value, gradients, measurement frame). Values cross-validated
  against pynrrd and geometry against ITK/SimpleITK: `dev_tools/check_nrrd_conversion.R`
  (29 checks, 0 failures). Fixtures generated by
  `dev_tools/generate_nrrd_test_data.py`; small ones shipped in `inst/extdata/nrrd`
  (126 tests in `tests/testthat/test-read_nrrd.R`). Writing is not implemented
  (deliberately, see the plan's scope note).
- Effort: M (actual: mostly in the header grammar and the integer width handling).

### II.5 [ ] AFNI BRIK/HEAD (read-only)
- nibabel is also read-only here. Real but narrower: AFNI users can convert to
  NIfTI. The `.BRIK.gz` variants and the header attribute grammar are the work.
- Effort: M-L.

### II.6 [ ] MRtrix `.mif` image read
- Needed for FODs/fixels/5D dMRI data and the fixel-directory format without
  shelling out to MRtrix. The tractography side is largely covered by TCK/TSF.
- Effort: M-L.

---

## III. Tier 3 -- cheap cleanups

- [x] `write.dti.tsf()` -- done. `write.dti.tsf()` accepts the `scalars` entry
  of `read.dti.tsf()`, an `fs.tracts` instance with a single column of scalars, a
  list of vectors, or one vector plus the new `lengths` parameter. The payload
  layout is the one MRtrix writes (NaN after *every* track, no Inf terminator) --
  it differs from the TCK layout, and MRtrix verifies the track count of a scalar
  file against the tractogram it is used with, so this matters. The header and
  payload builders are now shared with the TCK writer (`build.mrtrix.header()`,
  `write.mrtrix.streamlines()`), and the refactor is byte-identical for TCK.
  Verified with `dev_tools/check_mrtrix_tsf.R` (8 checks against MRtrix3 3.0.8,
  including `tsfvalidate` on all 4 data types and a byte comparison of the payload
  with a `tcksample`-written file from the real whole-brain opt data). Effort: S.
- [x] `.trk.gz` -- done. All five code paths handle compressed files
  (`read.dti.trk()`, `read.dti.trk.header()`, `get.dti.trk.endianness()`,
  `scan.trk.file()`, `trk.track.iterator()`), plus `detect.dti.tract.format()`,
  by using the existing `skip.connection.bytes()` for every seek and by reading
  the header as a whole. `write.dti.trk()` gained a `gzip` parameter. Verified
  with `dev_tools/check_trk_gzip.R` (8 checks, nibabel reads our compressed
  files), and with real test data (`extra_test_data/tracts/STR_R.trk[.gz]`, an
  MIT licensed XTRACT tractogram, added to the repo for exactly this purpose).
  Effort: S.
- [x] STL write -- done, and slightly more than planned: `write.fs.surface.stl()`
  writes both variants (binary by default), `write.fs.surface()` dispatches to it
  for `.stl`, `.stla` and `.stlb` (the missing dispatch used to write a FreeSurfer
  binary surface for a `.stl` file name, silently), the readers now return the
  face normals they documented but dropped, and the stale `misc3d` note is gone.
  Verified with `dev_tools/check_stl_conversion.R` (12 checks: VTK, FreeSurfer
  `mris_convert` and meshio). Effort: S.
- [ ] Connectome Workbench `.spec` -- how HCP file collections and structure
  mappings are declared alongside CIFTI/GIFTI. Effort: S-M.

---

## IV. Deliberately NOT planned (even though nibabel has them)

- **MINC-1/2** -- CIVET/MNI legacy, negligible overlap with the target audiences.
- **ECAT7** (PET) -- outside both target communities.
- **Philips PAR/REC** -- read-only, ultra-niche, nibabel has discussed dropping it.
- **DICOM** -- highest raw-data prevalence, but a huge effort for a worse result
  than `dcm2niix`; R's options (`oro.dicom`, `divest`) are unmaintained. If ever
  done, mirror nibabel's scope: read-only, uncompressed, explicit/implicit VR
  little-endian, Siemens/Philips mosaics only.
- **SPM99/SPM2 header quirks** -- plain NIfTI covers modern data.

---

## V. Cross-cutting concerns

### V.1 [ ] Lazy / memory-mapped volume access
nibabel's real advantage for the fMRI crowd is `ArrayProxy`/`LazyTractogram`:
lazy, memory-mapped, slice-on-demand access. A 4D BOLD series (1-2 GB, or HCP's
4D NIfTI) is painful to read eagerly, and `fs.volume` reads whole arrays.

- [ ] Lazy volume handle that maps `.nii`/`.mgh` and reads only requested volumes
- [ ] Revisit the `safety_checks.R` allocation guard for that code path
      (the streaming-must-not-trip-the-whole-file-guard lesson from TRK applies)
- `dti.track.iterator()` is the right precedent; apply the same idea to volumes
  and CIFTI. This unlocks the fMRI use case more than three extra formats would.

### V.2 Extension dispatch
`read.fs.morph()`/`write.fs.morph()` dispatch on the extension, and
`readable.files()` has a hardcoded precedence list. New formats must be
registered consistently there.

### V.3 Verification strategy
Cross-validate semantics against an independent implementation, as was done for
the TRK reader against nibabel. Prefer `dev_tools/` scripts for checks that need
real data, and keep large files out of the repo (CRAN 5 MB limit ->
`extra_test_data/`, excluded via `.Rbuildignore`).

### V.4 Repo constraints to remember
- Do **not** commit/push: leave all changes in the working tree for review.
- `devtools::document()` with the installed roxygen 7.3.3 (DESCRIPTION claims
  8.0.0) rewrites ~85 unrelated `man/*.Rd` files. After every `document()`:
  revert all modified tracked `man/` files whose content did not really change,
  then `git checkout -- DESCRIPTION`.
- Internal helpers get man pages (no `@noRd` in this repo), documented with
  `@keywords internal`.
- CHANGES entries are long and explanatory, one section per version.

---

## Gradients: detailed spec (item I.1)

### Formats to support

1. **FSL `.bvec`** -- text, 3 rows x N columns (x, y, z per volume) in the
   *voxel* coordinate system of the accompanying image. Some tools write N x 3,
   hence the transpose heuristic.
2. **FSL `.bval`** -- text, one b-value per volume. Usually 1 row x N columns,
   sometimes N rows x 1 column.
3. **MRtrix `.grad`** -- text, one row per volume with 4 columns
   (gradient x, y, z, b-value). The gradient direction is unit length and
   b<=0 rows represent b=0 volumes. (Verify the exact header/count line rules
   against the MRtrix docs before implementing.)
4. **HCP-style `bvals`/`bvecs`** -- space-separated, no extension; HCP stores
   bvecs as N x 3. Should be accepted by the same readers.

### Known pitfalls to handle explicitly

- **Ambiguous transpose**: a 3x3 file (N=3) is ambiguous; must warn rather than
  silently guess.
- **Voxel vs scanner space**: FSL bvecs are relative to the image voxel axes, so
  converting to/from MRtrix requires the image's sform/qform. This is why the
  conversion helper must take a volume/header, not just the bvec file.
- **Sign convention**: FSL treats the diffusion direction as symmetric and its
  tools may flip bvec signs; MRtrix treats bvecs as signed directions. Any
  conversion must document exactly what it does and must not silently flip.
- **b=0 rows** may be `[0, 0, 0]` or an arbitrary unit vector; a b=0 row must
  never be normalised into a fake direction.
- **Non-unit vectors** occur in the wild; do not silently renormalise on read,
  but report it.

### API sketch (to be finalised against repo conventions)

- `read.fsl.bvec(filepath, ...)` -> N x 3 matrix (one row per volume)
- `read.fsl.bval(filepath)` -> numeric vector of length N
- `read.mrtrix.grad(filepath)` -> N x 4 matrix (or a small S3 object)
- matching `write.*` functions
- consistency checker: bvec rows == bval length
- voxel/scanner conversion helper taking a NIfTI/MGH header

### Verification

- Round-trip tests (read -> write -> read) for all layouts, incl. 1-volume and
  ambiguous 3-volume files.
- Cross-check against a known published gradient table (e.g. a well-known
  dataset's bvals/bvecs) and against nibabel/DIPY semantics.
- Test that a b=0 row survives a round trip unmodified.

---

## Progress log

### Increment 1: gradient tables (2026-09-17) -- DONE

- New files: `R/read_dwi_gradients.R`, `R/write_dwi_gradients.R`,
  `tests/testthat/test-read_dwi_gradients.R`,
  `tests/testthat/test-write_dwi_gradients.R`, 15 new man pages.
- 7 new exports: `read.dti.bvec/bval/grad/gradients`,
  `write.dti.bvec/bval/grad`. Internal: `validate.dti.gradients` and six helpers.
- Tests: 140 new expectations; full suite 1648 pass, 0 fail, 3 pre-existing skips.
  `R CMD check`: 0 errors, 0 warnings, 1 pre-existing sandbox NOTE.
- Real test data (2.8 KB, from `~/develop/sub-01-derived`, QSIPrep output) copied
  to `extra_test_data/dwi/`; nothing added to `inst/extdata`, so the CRAN 5 MB
  limit is unaffected and the always-on tests are self-contained.

Findings from this increment, worth not rediscovering:

- **A real file mixes the layouts.** The QSIPrep subject writes the b-vectors as
  3 component lines (FSL layout) but the b-values as one value per line, so the
  layout has to be detected per file, not assumed from the format.
- **The square case is genuinely ambiguous, and the references disagree.**
  MRtrix3 transposes a bvec table when `rows != 3` and `cols == 3` and treats a
  3x3 as components; DIPY treats a 3x3 as volumes. We follow MRtrix3 and warn.
  For the MRtrix gradient table there is no ambiguity (the format defines one
  row per volume), so the warning is scoped to b-vectors only -- otherwise a
  perfectly normal 4-volume gradient table would trigger it.
- **nibabel cannot be used as a reference for either Tier 1 DWI item** (no
  bvec/bval reader, no TRX). DIPY and MRtrix3 are the references.
- **The conversion needs the transform without voxel sizes.** MRtrix3's
  `load_bvecs_bvals()` computes `grad = transform.linear() * bvecs` after
  flipping `bvecs` row 0 when `det(linear) > 0`. MRtrix transforms are in mm
  with unit axes, so the R equivalent must divide the voxel sizes out of
  `vox2ras`/sform; using the raw affine would silently rescale the directions.
- **`formatC(x, format = "g", digits = 15)` is wrong here.** With the default
  `width = NULL` it treats `digits` as the field width and right-pads every
  value with spaces, producing files like `"               1"`. Use
  `sprintf("%.15g", x)`.
- **`nibabel/streamlines/` has no TRX** as of 5.4.2-131-g699b9923, so item II.1
  is greenfield in both ecosystems.

### Increment 4: ANALYZE 7.5 + NIFTI v1 pairs (2026-09-22) -- DONE

- New files: `R/read_analyze.R`, `R/write_analyze.R`,
  `tests/testthat/test-read_analyze.R`, `tests/testthat/test-write_analyze.R`,
  `dev_tools/generate_analyze_test_data.py`, `dev_tools/nibabel_analyze_dump.py`,
  `dev_tools/check_analyze_conversion.R`, fixtures in `inst/extdata/analyze` (16
  files) and `extra_test_data/analyze` (fixtures + 11 reference dumps + the file
  FreeSurfer wrote).
- 7 new exports, 17 new man pages (internal helpers included, as this repo
  requires). Modified: `read_nifti1.R`, `read_nifti2.R`, `write_nifti1.R`,
  `nifti_common.R`, `nifti_to_mgh.R`, `read_fs_volume.R`, `write_fs_volume.R`.
- Checks: `dev_tools/check_analyze_conversion.R` (22 checks, 0 failures) against
  nibabel *and* FreeSurfer; 93 + 73 new tests.

Findings from this increment, worth not rediscovering:

- **`.hdr`/`.img` is two formats, not one.** The 348 byte header is shared, the
  field semantics are not: the last 4 bytes are `smin` in ANALYZE and the magic in
  NIFTI (`ni1` = pair, `n+1` = single file), and everything from offset 56 to 147
  (`vox_units`, `cal_units`, `dim_un0`, `funused1-3`, `compressed`, `verified`),
  from 252 (`orient`, `originator`, `generated`, ...) and from 328 (`intent_name`
  vs. `start_field` and friends) is different. Reading an ANALYZE file with the
  NIFTI reader returns zeroes for the NIFTI fields for files written by nibabel,
  but arbitrary data for files written by ANALYZE, SPM, AFNI or scanners.
- **ANALYZE cannot store the orientation, and no reader can recover it.** The
  format has no affine field and `pix_dim` gives only the voxel sizes, so the
  left/right orientation is undefined (the famous defect that NIFTI was created to
  fix). nibabel returns a *convention*: `diag(-zooms[0], zooms[1], zooms[2])` with
  the translation from the SPM origin (`originator` field, 0-based after `-1`) or
  from the image centre if there is none. Mirroring a brain silently is worse than
  returning no matrix, so the default here is *no* matrix, `spm = TRUE` opts in,
  and the check script verifies the opt-in against nibabel for every fixture.
- **SPM re-uses ANALYZE fields**: `funused1` is the data scale factor,
  `originator` holds 3 little endian int16 with the origin, and `<base>.mat` (a
  MATLAB v4 file) holds the true affine. nibabel's `nib.load` picks
  `Spm2AnalyzeImage` for ANALYZE files, so the SPM fields are the *default*
  behaviour in the Python ecosystem. See item II.2b for the `.mat` sidecar.
- **`orient` is a mess.** It is a 1 byte code in the spec (0=transverse
  unflipped ... 5=sagittal flipped), FreeSurfer sets it (writing 4 for a file it
  itself reports as 'coronal flipped'), nibabel ignores it for the affine, and
  implementations disagree about the axis mapping. Report as-is, never use it to
  guess an orientation.
- **FreeSurfer cannot auto-detect its own ANALYZE output.** `mri_convert -ot mgz
  x.hdr out.mgz` fails with 'cannot determine file type'; `-it analyze` is
  required, and the *data* file (`.img`) has to be passed. Documented in
  `write.analyze()`.
- **Two real bugs in the NIFTI layer, found by this work (both fixed here):**
  `read.nifti1.data()` read *unsigned* 8/16 bit data as signed (R's `readBin`
  default), silently returning -56 for a `uint8` value of 200 -- which includes
  every mask/label volume, e.g. the NIFTI file `mri_convert` writes for
  `mri/brain.mgz`; and `read.nifti1.data()` on a `.hdr` pair returned the *header
  file bytes* as voxel data. Both are the kind of silent wrong-data bug this
  package exists to avoid, and neither was caught by the existing tests because
  their fixtures have no values above the signed maximum.
- **`readBin(size = 4, signed = FALSE)` is not an option** (it warns and ignores
  the argument), so 32 bit unsigned data needs the same two's complement fixup as
  in the NRRD reader. Both now share `read.nifti.values()`.
- **nibabel repairs a wrong `bitpix` field in memory** when loading, so a dump
  written by nibabel does not describe the file. The generator records the file
  value separately (`dump file_bitpix`) and the reader warns while trusting
  `datatype`, which is what nibabel does as well.
- **The ANALYZE `originator`/`generated`/`patient_id` fields contain arbitrary
  bytes.** Decoding them as UTF-8 (which `read.fixed.char.binary()` does) can fail
  or return NA; decoding as ISO-8859-1 cannot. New internal helper
  `analyze.read.char.field()`, and the writer converts back to latin-1 so the
  bytes round trip.
- **`matrix()` vs. a dump of a matrix.** Reference dumps store matrices in
  row-major order (nibabel/numpy/spec order), R matrices are column-major, so
  every comparison needs a `t()`. This produced three bogus "geometry differs by
  30" failures in the check script before it was noticed.
- **Test data has to be verified at generation time.** Two fixtures were silently
  wrong until the generator got self-checks: a `uint16` fixture that overflowed
  (32767 + 40000 wrapped to 7231, hiding the very bug it was meant to catch) and a
  fixture whose `pixdim` stayed at 1,1,1 because nibabel derives it from the affine
  passed to `AnalyzeImage`, not from `set_zooms()`.

Findings from the `.mat` sidecar work (II.2b), worth not rediscovering:

- **The sidecar is the only reliable geometry of an ANALYZE file.** NIFTI was
  created partly because of this. Both nibabel (`Spm99AnalyzeImage`) and SPM read
  it, and it is *not* a convention, so unlike the SPM origin heuristic it can be
  used by default. FreeSurfer writes one next to every `-ot analyze` output (see
  `fs_tiny.mat` in `extra_test_data/analyze`).
- **The matrix in the file is in MATLAB's 1-based voxel space.** nibabel's
  `to_111` step (identity with `[:3,3] = 1`) adds the *row sums* of the rotation
  part to the translation. Skipping this is a one-voxel shift, i.e. up to several
  millimeters, and nothing in the file or the affine would look obviously wrong.
- **`M` and `mat` are different variables.** `mat` includes the flip of the first
  axis, `M` does not (nibabel: "the 'M' matrix does not include flips", and it
  applies `diag(-1,1,1,1)` for `M` because ANALYZE's `default_x_flip` is True).
  FreeSurfer writes **only `M`**, nibabel writes both. Treating them the same
  gives a mirrored image for every FreeSurfer file.
- **MATLAB v4 files are parseable in ~60 lines**: type/mrows/ncols/imagf/namelen
  (5 x int32), then the NUL-terminated name, then column-major values (the same
  order R uses). The byte order is not stored, so validate by requiring that the
  declared variables describe exactly the file size, and try both. v5+ files
  (compressed, "MATLAB 5.0 MAT-file" header) cannot be read this way; report them
  and ignore the sidecar instead of failing the whole read.
- **Round trip confirmed**: nibabel's `Spm99AnalyzeImage` writes `mat` as the
  affine in 1-based voxel space, and reading it back with our code reproduces the
  affine that was passed to nibabel exactly (verified for every fixture in the
  check script).
