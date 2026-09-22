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

### I.2 [ ] CIFTI-2 read + write for all 9 file types (incl. `.dconn`/`.pconn`)
Planned in detail, see the section "CIFTI-2: detailed spec" below. Sub-items:

- [x] I.2a NIfTI-2 header extensions: read + write (`write.nifti2(..., extensions)`)
- [x] I.2b CIFTI-2 XML reader (`read.cifti.header()`, all 5 mapping types)
- [x] I.2c dense files: `dscalar`, `dlabel`, `dtseries`, `dconn` (read + write) -- read side in increment 7, write side in increment 8. The generic writer `write.cifti()` covers all nine standard file types, the user-facing writers cover the three dense surface types; the parcellated convenience writers belong to I.2d.
- [x] I.2e (partly) native `read.fs.*.cifti()`: `read.fs.morph.cifti()`, `read.fs.series.cifti()` and `read.fs.parcellation.cifti()` are native and accept a file path directly; the `cifti` package is only used if a client passes one of its objects. The dispatch fix for `read.fs.morph()`/`read.fs.volume()` is still open.
- [x] I.2d parcellated files: `read.fs.connectome.cifti()`, `pscalar`/`ptseries`/`pconn`/`pdconn`/`dpconn` writers, parcels axis from annotations -- done in increment 9, see the progress log. The writers are `write.fs.connectome.cifti()` (the four connectome types) and `write.fs.parcellated.cifti()` (`.pscalar`/`.ptseries`), the axis comes from `cifti.axis.parcels.from.annot()` or from a template.
- [ ] I.2f large files: row-wise access for `.dconn` (contiguous reads) -- the seek based column selection of `read.cifti()` exists, the dedicated row reader and its docs are open
- [ ] I.2g test data, dev tools, check script against nibabel + Workbench, docs

Notes: the container is NIfTI-2 + an XML header extension (ecode 32), so no new
dependency is needed (`xml2` is already imported). The hard part is the
index-map/brain-model bookkeeping, not the container. Reading is currently
delegated to the `cifti` package (Soft dep, and its CRAN version needs the
caller to pre-read the file, `muschellij2/cifti#9`); `.dconn` (the standard HCP
resting-state output) cannot be read at all today - verified: both
`cifti::read_cifti()` and our wrappers fail on a `.dconn` written by nibabel
("Unrecognized or inconsistent voxel IJK sequence"). Effort: L.

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
- Internal helpers get man pages (no `@noRd` in this repo), documented with
  `@keywords internal`.

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

## CIFTI-2: detailed spec (item I.2)

Planned 2026-09-22, nothing implemented yet. Everything below was **verified on
this machine** against the references, because the CIFTI-2 specification itself
is not machine-readable: it is a scanned PDF attached to a NITRC forum post
(<http://www.nitrc.org/forum/forum.php?thread_id=4380&forum_id=1955>, appendix in
thread 4381), and the searchable CIFTI-1 wiki page
(<http://www.nitrc.org/plugins/mwiki/index.php/cifti:Cifti-1>) documents the
*previous* version, which uses different element and attribute names
(`NodeIndices`, `TimeStep`, `UnitsXYZ`, `Version="1.0"`) and is therefore only
useful for the general architecture, not for the details. The references used:

- **nibabel 5.4.2** (`~/develop/brain_atlases/.venv/bin/python`), module
  `nibabel.cifti2` (`cifti2.py`, `cifti2_axes.py`, `parse_cifti2.py`, ~3.8k
  lines): a complete CIFTI-2 reader/writer whose parser follows the spec element
  by element, including which children are legal where.
- **Connectome Workbench 2.2.1**
  (`~/software/connectome_workbench/workbench/bin_linux64/wb_command`), the
  reference *implementation*: `-cifti-help` documents the mapping types and the
  file types, `-list-commands` lists 65 CIFTI subcommands, many usable as
  verifiers (`-nifti-information`, `-cifti-convert`, `-cifti-parcellate`,
  `-cifti-correlation`, `-cifti-math`, `-cifti-export-dense-mapping`).
- **The official CIFTI-2 example files** in `extra_test_data/cifti/` (2.1 MB,
  PDDL): `dtseries`, `ptseries` and `dlabel`, written by Workbench 0.84 in 2014.
- **The `cifti` R package 0.5.0** (CRAN): what the package uses today.

### 1. The container: NIfTI-2 plus one header extension

A CIFTI-2 file is a NIfTI-2 file (540 byte header, magic `n+2\0\r\n\x1a\n`) whose
only payload variation is a **header extension with code 32** that holds the XML.
Verified byte layout (offsets are absolute; the values are what Workbench 2.2.1
and nibabel 5.4.2 write):

| offset | field | CIFTI-2 value |
| --- | --- | --- |
| 0 | `sizeof_hdr` | 540 |
| 4 | `magic[8]` | `n+2\0\r\n\x1a\n` |
| 12 | `datatype`, `bitpix` | 16/32 (float32) for dense data - also for `dlabel` |
| 16 | `dim[8]` | `6,1,1,1,1,M,N,1`: `dim[0]` = 6 (7 for a 3D matrix), `dim[1..4]` = 1, `dim[5]` = size of matrix dimension 0, `dim[6]` = size of dimension 1 |
| 104 | `pixdim[8]` | Workbench: all 1; nibabel: `0,1,1,1,1,1,1,1` (qfac 0); not used |
| 168 | `vox_offset` | 544 + extension size, and the data starts exactly there |
| 176 | `scl_slope`, `scl_inter` | 1, 0 |
| 344, 348 | `qform_code`, `sform_code` | 0, 0 (the geometry lives in the XML) |
| 500 | `xyzt_units` | Workbench 10 (mm + sec), nibabel 0; readers ignore it |
| 504 | `intent_code` | 3001-3012, one per file type, see the table below |
| 508 | `intent_name` | `ConnDense`, `ConnDenseSeries`, `ConnParcels`, `ConnParcelSries` (sic), `ConnDenseScalar`, `ConnDenseLabel`, `ConnParcelScalr`, `ConnParcelDense`, `ConnDenseParcel` |
| 525 | `unused_str[15]` | zeros. Note our reader calls the bytes at the end of the header "padding" and never reads the next field - both parts are wrong, see I.2a. |
| 540 | extension flag | `01 00 00 00` (first byte non-zero = extensions follow), else four zeros |
| 544 | extension | `int32 size`, `int32 ecode` (= 32), then the XML, NUL-padded |

Extension size rule (verified on four files, incl. both writers): `size` is a
multiple of 16, `size = 8 + length(content)`, and `content` is the XML followed by
at least one NUL (Workbench: 1-15, nibabel: 2). Examples: 349712 = 21857 x 16 with
12 NULs (official `dtseries`), 138288 with 6 (official `ptseries`), 944 with 2
(nibabel's `row_major.dconn.nii`). The data length is
`prod(dim[5:7]) * bitpix / 8`; e.g. the official `dlabel` has 779808 = 3 x 64984 x
4 bytes after `vox_offset` 398832 = 544 + 398288.

Two consequences: (a) our NIfTI-2 layer needs extension **read and write**
(I.2a - `write.nifti2()` currently pads zeroes up to `vox_offset = 544`, which is
a valid "no extensions" file, so nothing breaks), and (b) **gzipped CIFTI files
must not be supported**: the format forbids compression ("CIFTI files must not be
compressed", so that random access stays possible), so `.dscalar.nii.gz` is an
error, not a fallback.

### 2. The data layout: one positional rule for the whole format

**Reader:** `m <- matrix(read.nifti.values(...), nrow = dim[5], ncol = dim[6])`,
i.e. `m[i + 1, j + 1]` is the value for index `i` along the mapping with
`AppliesToMatrixDimension="0"` and index `j` along the mapping of dimension 1.
**Writer:** `dim[5] = nrow(m)`, `dim[6] = ncol(m)`, and the bytes are
`as.vector(m)`. R's column-major order *is* the file order; there is no transpose
anywhere in the format (a transposed reading is the most likely way to get this
item silently wrong - see the evidence below).

The naming trap: Workbench calls matrix dimension 0 the **ROW** dimension and
dimension 1 the **COLUMN** dimension, which is the opposite of how the bytes are
laid out (the dimension-0 index varies fastest inside a stored row). So for a
`.dtseries` "ROW is series, COLUMN is dense" means `dim[5]` = number of series
points and `dim[6]` = number of brainordinates, and `m` has one row per series
point. The *file type names* list the two types in reverse of the dimension order
(`pdconn` = "ROW is dense, COLUMN is parcels", verified: the file with
`dim[5]` = dense and `dim[6]` = parcels must be named `.pdconn.nii`, and Workbench
warns if it is not - while its own warning message then describes it as "dense by
parcels"), and the intent names do the same (`ConnDenseParcel` = 3010 is
`..._DENSE_PARCELLATED`, again with dimension 0 = dense). Bottom line: **derive
the layout from the XML, never from the file extension or the intent name.**

Evidence that the layout rule and the index bookkeeping are right, and that the
transposed variant is wrong: from the official Conte69 `dtseries` (raw bytes) plus
the official Conte69 `dlabel` (parcel vertex lists), I averaged the values of each
parcel and got the official Conte69 `ptseries` values exactly - parcel
`MEDIAL.WALL`, series 0: 1.43101 in both, and 1.39253 / 1.52133 for the next two
parcels; the transposed reading interleaves the two series (1.43101, 2.5155,
1.39253, 2.33944) and does not match. This single check covers the byte order, the
dense index mapping and the parcel vertex lists at once, and it is worth turning
into a test.

### 3. The XML: the elements we must read and write

```
CIFTI Version="2"
+- Matrix
   +- MetaData (0..1) ......... MD (0..N) -> Name, Value
   +- LabelTable (0..1) ....... CIFTI-1 legacy, some tools put the parcel labels here
   +- Volume (0..1) ........... CIFTI-1 legacy; CIFTI-2 puts it in the MatrixIndicesMap
   +- MatrixIndicesMap (1..N) . AppliesToMatrixDimension ("0", "1", "0,1", ...)
      |                         IndicesMapToDataType
      |                         + series only: NumberOfSeriesPoints, SeriesStart,
      |                           SeriesStep, SeriesExponent, SeriesUnit
      +- BrainModel (0..N) ..... IndexOffset, IndexCount, ModelType, BrainStructure,
      |                          SurfaceNumberOfVertices (surfaces only)
      |                          -> VertexIndices (0..1) | VoxelIndicesIJK (0..1)
      +- Parcel (0..N) ......... Name
      |                          -> Vertices (0..N, attr BrainStructure) | VoxelIndicesIJK (0..1)
      +- Surface (0..N) ........ BrainStructure, SurfaceNumberOfVertices
      +- NamedMap (0..N) ....... MapName, LabelTable (0..1), MetaData (0..1)
      |                          -> Label (Key, Red, Green, Blue, Alpha, X?, Y?, Z?)
      +- Volume (0..1) ......... VolumeDimensions="i,j,k"
                                 -> TransformationMatrixVoxelIndicesIJKtoXYZ
                                    (MeterExponent, 16 values, row-major, 4x4)
```

- `IndicesMapToDataType` is one of `CIFTI_INDEX_TYPE_BRAIN_MODELS`,
  `..._PARCELS`, `..._SERIES`, `..._SCALARS`, `..._LABELS` (exactly these five).
- `SeriesUnit` is one of `SECOND`, `HERTZ`, `METER`, `RADIAN`; the time of index
  `i` is `(SeriesStart + i * SeriesStep) * 10^SeriesExponent`.
- One MatrixIndicesMap element can apply to several dimensions
  (`AppliesToMatrixDimension="0,1"`); a `.dconn` has exactly one such element, and
  a `.pconn`/`.dconn` therefore repeats the same brain model / parcel list for both
  dimensions. When reading, a single map must be expanded to both dimensions;
  when writing, the same content must be emitted once.
- Numeric text content is whitespace separated, and `VoxelIndicesIJK` holds three
  values per voxel: **nibabel writes them all on one line, Workbench one voxel per
  line**. A reader must accept both (split on any whitespace, require exactly
  `3 * IndexCount` values). This is not a theoretical detail: it is why the `cifti`
  R package (which requires one or three values per line) fails on *every* voxel
  model file written by nibabel - verified with `cifti::read_cifti()` on nibabel's
  `row_major.dconn.nii`, which errors with "Unrecognized or inconsistent voxel IJK
  sequence", and which is also why `.dconn` "cannot be read at all today". Our
  writer should use Workbench's one-voxel-per-line layout, which both other
  implementations accept.
- `Parcel` has **no index attribute in CIFTI-2**: the index is the position in the
  list (nibabel's `Cifti2Parcel` has no `index` field at all). Never reorder
  parcels, and do not let an `Index` attribute found in some other tool's output
  override the position.
- The transform matrix replaces CIFTI-1's `UnitsXYZ` by `MeterExponent`
  (Workbench writes `-3`, i.e. mm); it is stored row-major.
- `Version` must be `"2"`. A CIFTI-1 file has a different element set and must be
  rejected with a pointer to `wb_command -cifti-convert -version-convert in out 2`
  (that is exactly how the official example files were produced, see their
  provenance metadata).
- The XML header declaration is optional (Workbench 2.2.1 writes
  `<?xml version="1.0" encoding="UTF-8"?>`, nibabel `<?xml version="1.0" ?>`, the
  2014 example files none at all) and the whitespace is arbitrary, so always
  compare *parsed* structures, never strings.

### 4. The nine standard file types

Each row verified by creating such a file with Workbench 2.2.1 and dumping its
header; `dim0` = the mapping of `dim[5]` (Workbench's ROW), `dim1` = `dim[6]`
(COLUMN):

| extension | dim0 | dim1 | intent code | intent name |
| --- | --- | --- | --- | --- |
| `.dconn.nii` | BRAIN_MODELS | BRAIN_MODELS | 3001 | `ConnDense` |
| `.dscalar.nii` | SCALARS | BRAIN_MODELS | 3006 | `ConnDenseScalar` |
| `.dtseries.nii` | SERIES | BRAIN_MODELS | 3002 | `ConnDenseSeries` |
| `.dlabel.nii` | LABELS | BRAIN_MODELS | 3007 | `ConnDenseLabel` |
| `.pconn.nii` | PARCELS | PARCELS | 3003 | `ConnParcels` |
| `.pscalar.nii` | SCALARS | PARCELS | 3008 | `ConnParcelScalr` |
| `.ptseries.nii` | SERIES | PARCELS | 3004 | `ConnParcelSries` |
| `.pdconn.nii` | PARCELS | BRAIN_MODELS | 3010 | `ConnDenseParcel` |
| `.dpconn.nii` | BRAIN_MODELS | PARCELS | 3009 | `ConnParcelDense` |

**Correction (2026-09-22, found by increment 8):** the two rows above were swapped in the
first version of this table. The files that Connectome Workbench 2.2.1 writes (and the
generator script dumps) state it clearly: `.dpconn.nii` is `dim[5]` = dense (BRAIN_MODELS)
and `dim[6]` = parcels with intent code 3009 `ConnParcelDense`, and `.pdconn.nii` is
`dim[5]` = parcels and `dim[6]` = dense with intent code 3010 `ConnDenseParcel`. `R/write_cifti_axes.R`
now carries the verified table, and the writer refuses a file name whose type contradicts
the axes, so this cannot silently produce a misnamed file again.

`.ppseries`/`.ppscalar` (3011/3012, parcellated x parcellated, rarely used) fall
out of the generic implementation for free. The smallest one I generated this way
is 2164 bytes (1 parcel x 1 parcel `pconn`), the 10 x 10 `dconn` is 2640 bytes, so
these make excellent shipped test data (section 11).

### 5. Brain model bookkeeping (the actual hard part)

- Every `BrainModel` covers a contiguous index range `[IndexOffset,
  IndexOffset + IndexCount - 1]` of *its* dimension. **A structure may appear in
  several brain models** (surface part plus volume part, or split groups), so the
  reader must not assume one brain model per structure or per hemisphere; the
  public accessor should return a table of ranges instead
  (`structure, model_type, index_offset, index_count, surface_number_of_vertices`).
- `IndexCount` is what the file contributes, `SurfaceNumberOfVertices` is the size
  of the full surface - and they differ for grayordinates files, which drop the
  medial wall. The official Conte69 `dtseries` has 30424 (lh) and 30527 (rh)
  indices for 32492 vertex surfaces, so 2068 + 1965 vertices are absent.
- `VertexIndices` may be absent (all vertices of the surface, `IndexCount` must
  equal `SurfaceNumberOfVertices`), and `VoxelIndicesIJK` may be absent (all voxels
  of `VolumeDimensions`) - both are legitimate and mean "everything".
- Vertex indices are 0-based; voxel indices are 0-based IJK triplets in the grid
  of the original volume, mapped to XYZ (mm) by the transform times
  `10^MeterExponent`.
- The per-vertex API keeps its current contract: the returned vector has length
  `SurfaceNumberOfVertices` with `NA` where the file has no value (that is what
  makes a grayordinates file usable with a full 32k surface, and what
  `read.fs.morph.cifti()` already does today). Volume structures cannot be
  returned as a per-vertex vector: return the index/affine information instead of
  silently producing something.
- Structure names must accept `CORTEX_LEFT`, `CIFTI_STRUCTURE_CORTEX_LEFT` and
  nibabel's `CortexLeft` spelling, plus the existing `lh`/`rh` aliases (the
  structure table is in `nibabel.cifti2.CIFTI_BRAIN_STRUCTURES` and in
  `src/Common/StructureEnum.cxx` of Workbench).

### 6. Parcels

- A parcel is a name plus vertices per structure (a parcel spanning both
  hemispheres is the normal case: the official `ptseries` has `<Vertices>` for
  `CORTEX_LEFT` and `CORTEX_RIGHT` in each parcel) and optionally voxel indices.
- The index is the position; names are the only labels in a parcellated file
  (Workbench writes no `LabelTable` next to `PARCELS`; the names come from the
  `LabelTable` of the `.dlabel` that was parcellated).
- The medial wall is a parcel like any other when the source parcellation has a
  label for it ("MEDIAL.WALL" is parcel 0 of the official `ptseries`) - never
  filter it out.
- To write a parcellated file one needs a parcels axis, from either a template
  CIFTI file (recommended, same idea as `-cifti-create-*-from-template`) or a
  parcellation in this package's own types: two `fs.annot` objects, grouped by
  label *name* across hemispheres (the same normalization that was needed for the
  yabplot atlases, where lh/rh carry `L_`/`R_` prefixes).

### 7. API sketch

Naming follows the package (readers `read.fs.*`, writers `write.fs.*`, internal
helpers get man pages, `@family cifti`):

- low level: `read.cifti.header(filepath)` -> an `fs.cifti` object that mirrors
  the XML losslessly (`matrix$metadata`, `matrix$indices_maps`, each with `dims`,
  `type`, `brain_models`, `parcels`, `surfaces`, `volumes`, `named_maps`,
  `series`) plus the NIfTI-2 header; `cifti.structures(cii)`,
  `cifti.parcels(cii, dim)`, `cifti.series.info(cii)`, `cifti.label.table(cii)`.
- data: `read.cifti(filepath, rows = NULL, columns = NULL)` (header + the matrix;
  the selection is applied while reading, not after), `read.cifti.rows(filepath,
  indices, ...)` for the row-wise access of section 8.
- the existing readers become native and keep their signatures and semantics:
  `read.fs.morph.cifti()`, `read.fs.parcellation.cifti()`,
  `read.fs.series.cifti()` - now also accepting a *file path* directly, which
  removes the documented "read the file with the `cifti` package first"
  workaround (`muschellij2/cifti#9`).
- new: `read.fs.connectome.cifti(filepath, ...)` -> the `dconn`/`pconn` matrix
  plus, for `pconn`, the parcel names; this is the piece that does not exist at
  all today.
- writing: `write.cifti(filepath, data, header = NULL, template = NULL, ...)` as
  the single generic entry point (the file type follows from the axes and/or the
  file name, and is validated: a `.pdconn.nii` name with the wrong axis order is
  an error, not a warning as in Workbench), plus axis builders
  `cifti.axis.brain.models()`, `cifti.axis.parcels()`, `cifti.axis.series()`,
  `cifti.axis.scalars()`, `cifti.axis.labels()` and
  `cifti.header.from.axes(axes, metadata = NULL)` (the equivalent of nibabel's
  `cifti2_axes.to_header()`, which is the cleanest way to keep the "which
  dimension is which" logic in one place).
- convenience writers for the common cases, all accepting a `template`
  (recommended: the grayordinates mapping of a real HCP file cannot be invented):
  `write.fs.morph.cifti()` (`dscalar`), `write.fs.parcellation.cifti()`
  (`dlabel`), `write.fs.series.cifti()` (`dtseries`),
  `write.fs.connectome.cifti()` (`dconn`/`pconn`, type from the file name or an
  explicit argument), `write.fs.parcellated.cifti()` (`pscalar`/`ptseries`).
- dispatch fixes (small but they are real bugs today): `read.fs.morph()` on a
  `.dscalar.nii`/`.dtseries.nii` currently treats it as a NIfTI file and
  **silently returns the raw matrix as a morph vector** (verified: a 121902
  element vector for the official `dtseries`), and `read.fs.volume()` fails with
  the unrelated message "This is not a one-file NIfTI format". Both should detect
  a CIFTI (magic `n+2` plus intent >= 3000 or extension code 32) and either do the
  right thing or error with a pointer to the CIFTI reader.

### 8. Large files: `.dconn` is 9-38 GB

Reading a real HCP `.dconn` eagerly is impossible, so this is a feature, not an
optimization. The layout makes it easy: a *stored* row is one index of dimension 1
(one grayordinate), and its `dim[5]` values are contiguous, so
`read.cifti.rows(filepath, indices)` = one seek plus one `readBin` per requested
index. The eager reader must consult the existing `validate_allocation_size()`
guard and its error message should point at the row-wise reader instead of just
refusing. Writing large files can stay simple (`writeBin` of the whole matrix) with
an optional chunked writer (`rows_per_chunk`) as a later extension. Do not exploit
the symmetry of connectomes when reading or writing: the file stores the whole
matrix.

### 9. Deliberately not supported

- **CIFTI-1** (`Version="1.0"`, `NodeIndices`/`TimeStep`/`CIFTI_ROOT`): reject with
  the `-cifti-convert -version-convert` hint.
- **The CIFTI-1 sparse (compressed row storage) and per-row GZIP
  representations**: their intent codes were never standardised, no current tool
  writes them; detect and reject by intent code, do not guess.
- **gzipped CIFTI files** (forbidden by the format, see section 1).
- The `-cifti-convert -to-gifti-ext` bridge (GIFTI is item II.3, and the bridge is
  a Workbench convenience, not a file format).
- Fabricating Workbench-style provenance metadata. Metadata that is in the file is
  preserved on rewriting; nothing is invented, so round trips stay comparable.

### 10. Verification strategy

Oracles, in decreasing strength:

1. **Semantic reproduction** (writer-independent, catches exactly the silent
   errors this item is about): parcellate the official `dtseries` with the
   official parcels and compare with the official `ptseries` (section 2 shows this
   works and is decisive); correlate the official `dtseries` rows and compare with
   Workbench's `-cifti-correlation` output; export the dense mapping with
   `-cifti-export-dense-mapping` and compare with our brain model table.
2. **Workbench reads our files**: `wb_command -nifti-information <file>
   -print-header -print-matrix -print-xml` (note: a `-print-*` option is required,
   `-help` does not exist, run a subcommand without arguments for its usage),
   `-cifti-convert -to-text` (values as text), `-cifti-stats`, `-cifti-transpose`,
   `-cifti-parcellate`, `-cifti-math`.
3. **nibabel** reads our files and we read nibabel's (values, every XML field, the
   computed axes via `cifti2_axes`); its parser is strict, so it doubles as a
   schema check. Caveat (found in increment 6): its axis layer, and therefore
   `nibabel.load()`, refuses files in which the same structure has both a surface
   and a volume model ('Undefined vertex indices found for surface elements'),
   which Workbench writes. Use `cifti2.parse_cifti2.Cifti2Parser` on the extension
   payload for those, and do not treat a nibabel failure there as a file defect.
4. **We read Workbench-written files** (values and XML), including the files
   generated for the test data (section 11).
5. **The `cifti` R package** for the types it supports (it stays in `Suggests` for
   this purpose only) - a regression check that the existing user-visible results
   do not change.
6. **Round trips**: read -> write -> read, comparing parsed headers and values
   (never bytes: XML formatting and metadata order differ legitimately), plus
   "we wrote it, Workbench reads it, Workbench's values equal the input".
7. **Byte level**: the extension size/padding/`vox_offset` rules of section 1, as
   a dump comparison like the ones used for NRRD and ANALYZE.

To be built like the other formats: `dev_tools/generate_cifti_test_data.py`
(fixtures, nibabel), `dev_tools/cifti_dump.py` (reference dumps: dims, intent,
extension layout, canonicalized XML, value statistics), `dev_tools/check_cifti_conversion.R`
(the checks above; dumps make the nibabel part runnable without Python, the
Workbench part is live), and hand-written tests.

### 11. Test data

- `inst/extdata/cifti/` (shipped with the package, a few KB each): the small files
  written by Workbench 2.2.1 that I generated while planning -
  `dscalar`, `dlabel`, `dtseries`, `dconn`, `pscalar`, `ptseries`, `pconn`,
  `dpconn`, `pdconn` on a synthetic 10 vertex surface, plus (a) a nibabel-written
  voxel-model file with all `VoxelIndicesIJK` on one line (the layout that breaks
  `cifti`), and (b) a truncated file for the error paths. Their exact commands go
  into the generator script so they can be regenerated; the provenance metadata
  they contain (paths of this machine) should be replaced by something neutral
  when committing, or the files should be regenerated in a fixed directory.
- `extra_test_data/cifti/`: the three official example files (already in the
  repo), plus `expected/` reference dumps, plus - very valuable because it has
  **real geometry** and a **reduced dense mapping** - a small `.pconn` built from
  the official `dtseries` + `dlabel`
  (`-cifti-parcellate ... COLUMN` then `-cifti-correlation`, 54 x 54 = ~11 KB) and
  a small `.dconn` built by restricting the dense mapping to ~300 grayordinates
  first (`-cifti-restrict-dense-map <in> COLUMN <out> -left-roi/-right-roi` with a
  synthetic ROI metric, then `-cifti-correlation`, ~360 KB). Those two exercise
  "IndexCount < SurfaceNumberOfVertices", non-contiguous vertex indices and the
  symmetric matrix case.
- Not shippable, and therefore only handled by the row-wise reader and documented:
  a real HCP `.dconn` (9-38 GB) and the HCP grayordinates templates.

### 12. Sub-items, sequencing and effort

- **I.2a** (S): NIfTI-2 header extensions - reader returns them, `write.nifti2()`
  writes them, `vox_offset` bookkeeping, the wrong "padding" comment fixed, tests
  (`test-read_nifti2.R`, `test-write_nifti2.R`).
- **I.2b** (M): the XML model - parser for all five mapping types and all elements
  of section 3, `read.cifti.header()`, structural validation with clear errors
  (CIFTI-1, unknown type, index/graph inconsistencies), tests with the fixtures.
- **I.2c** (M): dense data - `read.cifti()`, brain model table, the per-structure
  and per-vertex accessors, volume structures, native
  `read.fs.morph.cifti()`/`read.fs.series.cifti()`, `dscalar`/`dlabel`/`dtseries`/
  `dconn` writers.
- **I.2d** (M): parcels - parcels axis (surface + volume parcels), `dlabel` label
  tables, `read.fs.parcellation.cifti()`, `read.fs.connectome.cifti()`,
  `pscalar`/`ptseries`/`pconn`/`pdconn`/`dpconn` writers, building a parcels axis
  from annotations.
- **I.2e** (S): integration - remove the hard `cifti` requirement from the code
  paths, the CIFTI detection in `read.fs.morph()`/`read.fs.volume()`, docs
  (`?read.cifti`, README, CHANGES).
- **I.2f** (S): row-wise access for large files (`read.cifti.rows()`), allocation
  guard message, tests with a synthetic large-header file.
- **I.2g** (M): the dev tools, the check script against nibabel and Workbench, the
  shipped fixtures, the extra test data, and the plan/README/CHANGES updates.
- Effort: **L** overall (the earlier "M-L" estimate holds; the reading side and its
  verification dominate, the container itself is small because the NIfTI-2 layer
  and `xml2` are already there).
- Risks: the spec is not machine-readable, so the XML grammar rests on nibabel's
  parser plus Workbench's behaviour - mitigated by testing against both; the
  `cifti` package must not regress the existing behaviour (section 10.5); the
  naming/dimension traps of sections 2 and 4 are the most likely source of a
  silent transposition bug, which is why the semantic reproduction check comes
  first.

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

### Increment 5: NIFTI v2 header extensions (2026-09-22) -- DONE (item I.2a)

- New file `R/nifti2_extensions.R` (`nifti2.extension()`,
  `nifti2.get.extension()`, `nifti2.extension.text()` exported; the size, the
  reader, the writer and the NUL handling as internal helpers with man pages).
- `read.nifti2.header()` returns the extensions in a new `extensions` field,
  `write.nifti2()` gained an `extensions` parameter (falling back to the field of
  the header, so a header can be read and written back without losing them),
  `ni2header.template()` no longer claims the NIFTI v1 magic, and `pix_dim` of
  the template is no longer all zeroes.
- New `dev_tools/nibabel_nifti2_dump.py` (dumps a file with nibabel, can also
  write a reference file) and `dev_tools/check_nifti2_extensions.R` (28 checks,
  0 failures): nibabel reads our files with and without extensions, compressed
  and uncompressed, and reports the exact payloads; we read a nibabel-written
  file; the XML metadata of the three official CIFTI-2 example files written by
  Connectome Workbench matches nibabel byte for byte; Connectome Workbench reads
  our file and agrees on the data offset.
- New tests `tests/testthat/test-read_nifti2_extensions.R` and
  `test-write_nifti2_extensions.R` (71 expectations). Full suite: 2628 pass,
  0 fail, 2 pre-existing skips. `R CMD check`: 0 errors, 0 warnings, 0 notes.

Findings from this increment, worth not rediscovering:

- **The magic string of a NIFTI v2 file is `n+2\0\r\n\032\n`, and older versions
  of this package wrote `n+1`.** Both reference implementations reject such a
  file (nibabel: 'magic string n+1 is not valid'; Connectome Workbench:
  'incorrect magic', exit code 255), i.e. every NIFTI-2 file this package has
  written so far was unreadable outside R. Fixed, and reading such a file now
  warns. The template also had `pix_dim` all zeroes, which nibabel repairs on
  load (with a warning), so other software saw a header different from the one we
  wrote; it is all 1.0 now, like nibabel and Workbench.
- **FreeSurfer's `mri_convert` cannot read NIFTI-2 files at all**, not even ones
  written by nibabel ('niiRead(): bad magic number'). It is therefore *not* a
  reference implementation for NIFTI-2, only for NIFTI v1 and ANALYZE.
- **Extension area layout**: 4 flag bytes at offset 540 (first byte non-zero iff
  extensions follow), then per extension a 4 byte size (total, always a multiple
  of 16) and a 4 byte code, then the payload; the data starts at `vox_offset`,
  which is exactly `544 + sum(sizes)` with no extra padding. The size is
  `(length(payload) + 23) %/% 16 * 16` (nibabel's rule, `content + 8` rounded up
  to 16). Workbench always pads with at least one NUL, nibabel does not guarantee
  one, and **both strip trailing NULs when reading**, so we do too (which makes a
  payload comparison against nibabel exact, and lets a rewritten file end up with
  a different but valid size).
- **Reading the magic with `read.fixed.char.binary()` gives `n+2\r\n\032\n`**
  (7 characters): that helper removes NUL bytes, so comparisons must use
  `substr(magic, 1, 3)`.
- nibabel reports `hdr['dim'][0]` as the number of used dimensions (3 for our
  3D test file), not as the highest used index.
- For a CIFTI file, `nibabel.load()` returns a `Cifti2Image` whose `.header` is a
  *CIFTI* header with no `dim` field; the NIFTI header is at `.nifti_header`. The
  payload of a CIFTI extension is parsed into an object by nibabel, so the raw
  bytes are only available via the private `._raw` attribute (a useful trick for
  a check tool, but nothing to build on).
- Connectome Workbench as a verifier: `wb_command -nifti-information <file>
  -print-header` prints the header and exits 255 with 'incorrect magic' for a bad
  file; at least one `-print-*` option is required and the file must be a valid
  NIFTI, `-print-xml` is CIFTI-only.

### Increment 6: CIFTI-2 XML reader (2026-09-22) -- DONE (item I.2b)

`R/read_cifti_header.R`: `read.cifti.header()` plus `cifti.structures()`,
`cifti.parcels()`, `cifti.series.info()`, `cifti.label.table()` and
`print.fs.cifti()`. 276 new tests in `tests/testthat/test-read_cifti_header.R`, 13
new fixtures in `inst/extdata/cifti/` (see below), and the tools
`dev_tools/generate_cifti_test_data.py` (fixture generator, writes the input
files with nibabel and then runs Workbench), `dev_tools/cifti_dump.py` and
`dev_tools/cifti_dump.R` (nibabel and fs.cifti reference dumps for a text diff).

Findings worth not rediscovering:

- **The R `dim` vector index is not the NIFTI `dim[i]` index.** `read.nifti2.header()`
  stores the field in an R vector, so CIFTI matrix dimension 0 (NIFTI `dim[5]`) is
  `niiheader$dim[6]`, and the number of matrix dimensions is `dim[0] - 4`. The first
  version of `cifti.matrix.dim.sizes()` used `dim[5 + i]` and produced sizes that
  were off by one, which the new consistency checks caught immediately (they
  compare every mapping against the dimension size) - a good argument for having
  them.
- **`AppliesToMatrixDimension` and `VolumeDimensions` are comma separated**
  (`"0,1"`, `"4,4,4"`), while the index lists are whitespace separated. The integer
  list parser splits on both, otherwise `"0,1"` is reported as 'not an integer'.
- **A single `MatrixIndicesMap` can apply to both dimensions** (`dconn`, `pconn`):
  the mapping is stored once with `dims = c(0, 1)`, and the accessors accept either
  dimension. All nine file types are covered by the parser, including `.dpconn` and
  `.pdconn`, whose names list the dimensions in the reverse order of the file - the
  fixtures confirm once more that the dimension order must come from the XML.
- **SCALARS maps can contain `NamedMap` elements** (one per scalar, with `MapName`
  and a `MetaData` that holds a `PaletteColorMapping`), LABELS maps must contain one
  `NamedMap` with a `LabelTable` per label map, and `MapName` can be empty (Workbench
  writes an empty element when a scalar map has no name). Metadata values can contain
  escaped XML as *text*, which `xml2::xml_text()` unescapes.
- **`-version-convert` is gone from Workbench 2.x** (`wb_command -cifti-convert
  -version-convert ...` errors with 'Unexpected parameter'), so a CIFTI-1 file cannot
  be produced or converted with the installed Workbench anymore. The error message
  for CIFTI-1 files mentions the option and that only older Workbench versions have
  it. A CIFTI-1 file is a *NIFTI-1* file containing CIFTI XML, so the tests simulate
  one by appending the string to a NIFTI-1 file.
- **nibabel 5.4.2 cannot load a CIFTI-2 file in which the same structure has both a
  surface and a volume brain model** (`Cifti2Image.__init__` -> `get_data_shape()` ->
  `BrainModelAxis.__init__()` raises 'Undefined vertex indices found for surface
  elements'). Workbench writes such files, our reader reads them, and the reference
  dump tool falls back to nibabel's raw XML parser (`Cifti2Parser().parse(xml_bytes)`
  on the extension payload) for them. `-cifti-create-dense-scalar -left-metric <m>
  -roi-left <roi> -right-metric <m> -volume <vol> <labelvol>` produces one if the
  structure label volume labels cortex voxels.
- **The two implementations agree exactly.** A text diff of the `dev_tools/cifti_dump.py`
  and `dev_tools/cifti_dump.R` output is empty (0 differing lines) for all 13 fixtures
  and for the three official Conte69 files (547 dump lines each, including 60951
  vertex indices, 54 parcels with their vertex lists and 3 x 96 labels). The official
  files were written by a different tool (Workbench 0.84, 2014) than the fixtures
  (Workbench 2.2.1), so this covers two writers and two eras.
- The fixture *provenance* metadata contains the absolute paths of the generating
  machine (Workbench always writes `Provenance` and `WorkingDirectory`). The
  generator writes into a fixed build directory (`/tmp/cifti_fixtures/build`, so the
  files are reproducible), and the plan of record is to neutralize the paths in
  I.2g by rewriting the extension with `write.nifti2()` (which is also a nice
  end-to-end check of our extension writer against Workbench).
- Write support was *not* part of this increment: the `fs.cifti` object keeps
  everything needed to rebuild the XML, but unknown attributes of the input are not
  preserved, so a read -> write round trip is compared on *parsed* structures, as
  section 10 of the spec requires.
- **`R CMD check` is clean** (0 errors / 0 warnings / 0 notes, R 4.6.1, roxygen2
  8.0.0, full suite: 2904 pass / 0 fail / 2 skip). One trap worth remembering: the
  roxygen markdown of `` `dim[5]`, `dim[6]` `` becomes `dim\link{5}, dim\link{6}`
  (shortcut reference links), and a `` `foo()` `` naming a function that does not
  exist yet becomes `\code{\link{foo}}` -- both are reported as "Missing link(s)
  in Rd file ..." by the cross-reference check. Bracketed index notation was
  replaced by prose ("entries 5, 6, ... of the `dim` field") and the forward
  reference to `read.cifti()` (increment 7) by `\code{read.cifti}`.

### Increment 7: CIFTI-2 data reader (2026-09-22) -- DONE for reading (item I.2c, read part)

Files: `R/read_cifti.R` (new, ~600 lines incl. roxygen), rewired `R/cifti.R`,
`tests/testthat/test-read_cifti.R` (new, 287 tests), new tests in
`tests/testthat/test-cifti2.R`, extended `dev_tools/cifti_dump.{py,R}` (they now
also dump the data values), new `dev_tools/check_cifti_conversion.R`. No new
dependency (`xml2` and the NIfTI-2 reader of I.2a are reused).

Public API added: `read.cifti(filepath, rows, columns)` -> `fs.cifti.data`
(`$header` + `$data`), `print.fs.cifti.data()`, `cifti.structure.data(x,
structure, dim)`, `cifti.grayordinates(cii, dim)`, `cifti.dim.labels(cii, dim)`.
`read.fs.morph.cifti()`, `read.fs.series.cifti()` and
`read.fs.parcellation.cifti()` are native now and accept a path, an `fs.cifti`
header or an `fs.cifti.data` object; objects of the `cifti` package keep working
(the old helpers are kept as `.cifti.legacy.*`).

Findings worth keeping:

- **The `Surface` element of a `MatrixIndicesMap` is optional, and no file we
  have contains one.** Every file written by Workbench (including the official
  2014 Conte69 files) reports the surface size only in the
  `SurfaceNumberOfVertices` attribute of each brain model. `read.cifti.header()`
  parses the `Surface` elements, so the surface size for the reconstruction has
  to fall back to the brain model attribute (nibabel's `BrainModelAxis` does the
  same); the fallback errors if several brain models of one structure disagree.
  This was the first thing the new tests caught.
- **A single `MatrixIndicesMap` can apply to both matrix dimensions** (`.dconn`,
  `.pconn`, `.pdconn`): "how many brainordinate dimensions does this file have"
  must count *dimensions covered by* brain-model mappings, not mapping elements.
  Counting elements made a `.dconn` look like a normal dense file, which the
  test for the ambiguous-dimension error caught.
- **The Workbench text output is transposed relative to `read.cifti()`**: it
  writes one line per index of matrix dimension 1, while the data array is
  dim0-first (nibabel's order). Comparing needs `t()`.
- **That text output has 6 significant digits**, so the Workbench value
  comparison needs a tolerance: the largest relative difference is 2.1e-6 for
  the fixtures and 4.7e-6 for the official `.dtseries` (the residual is exactly
  the text rounding, e.g. Workbench writes `154.833` where the float32 value is
  `154.8333`).
- nibabel 5.4.2 cannot load the mixed surface+volume fixture at all, so it cannot
  dump its data either; the dump tool now falls back to reading the raw bytes at
  `vox_offset` with numpy (reported on stderr, since that assumes the storage
  order instead of proving it). Workbench verifies that file's values.
- Verification results (all with the shipped fixtures plus the three official
  files, 16 files in total): the nibabel dump comparison is **0 differing lines**
  (925 lines each, including all data values); the Workbench text comparison
  passes for all 16 files; the native readers agree exactly with the `cifti`
  package on the official files (morphometry, series, label keys and label table);
  and the decisive semantic check passes: **averaging the official `.dtseries`
  over the vertices of each parcel reproduces the official `.ptseries`** (max
  absolute difference 1.2e-7 over all 108 values, i.e. float32 precision). That
  single check covers the byte order, the dense index mapping, the surface vertex
  indices and the parcel vertex lists of both hemispheres at once.

Design decisions:

- `read.cifti()` returns the array in file order (dimension 0 first), which is
  what nibabel returns, and names the dimensions with the axis labels. `rows` and
  `columns` are 1-based R indices into that array (the XML indices stay 0-based
  and are documented as such); the images are scanned for a transposed reading by
  the tests.
- Column selection is seek based (one contiguous read per column, no full read of
  the file), so a submatrix of a huge `.dconn` can be read; row selection still
  reads the whole file and is documented as such. The allocation guard message now
  points at `columns` instead of only refusing. This is most of I.2f; what is
  missing there is the `read.cifti.rows()` convenience entry point and its docs.
- Volume structures are *not* expanded to per-vertex data: `cifti.structure.data()`
  returns the values with the 0-based IJK voxel indices, the volume dimensions and
  the transformation matrix, and `read.fs.morph.cifti()` keeps erroring for them
  (with a message that explains the alternative now).
- A volume brain model without `VoxelIndicesIJK` (legal, meaning "all voxels") is
  expanded in raster order (first index fastest). Both reference implementations
  refuse such files in practice, so the writer (next step) will always write the
  voxel indices explicitly, and this branch is only a reader leniency.
- The `Surface`/`Volume` fallback and the dimension counting are the two places
  where the spec is less explicit than the files are; both are now recorded here
  and covered by tests.

Still open in I.2c: the XML writer (axis objects, `cifti.header.from.axes()`,
`write.cifti()` and the `write.fs.*.cifti()` convenience writers for `dscalar`,
`dlabel`, `dtseries` and `dconn`), and the round trip checks that come with it.

### Increment 8: CIFTI-2 writer (2026-09-22) -- DONE (item I.2c, write part)

Files: `R/write_cifti_axes.R` (axis builders, file type table), `R/write_cifti.R` (XML
emitter, `write.cifti()`), `R/write_cifti_fs.R` (the user-facing writers),
`tests/testthat/test-write_cifti.R` (new, 234 tests). Item I.2c is complete; I.2d is now
only about the parcellated convenience writer (`write.fs.connectome.cifti()`, building a
parcels axis from annotations) and the remaining `pscalar`/`ptseries`/`pconn` helpers,
because the generic writer already handles all nine file types.

Public API added: `write.cifti(filepath, data, axes, template, metadata)`,
`cifti.header.from.axes()`, `cifti.file.type.for.axes()`, `cifti.axis.from.template()`,
the builders `cifti.axis.brain.models()`, `cifti.brain.model.surface()`,
`cifti.brain.model.volume()`, `cifti.volume()`, `cifti.axis.parcels()`, `cifti.parcel()`,
`cifti.axis.series()`, `cifti.axis.scalars()`, `cifti.axis.labels()`, and the writers
`write.fs.morph.cifti()`, `write.fs.series.cifti()`, `write.fs.parcellation.cifti()`.

Findings worth keeping:

- **Connectome Workbench requires a child element in `BrainModel`**: a model without a
  `VertexIndices` or `VoxelIndicesIJK` element makes it abort with "CIFTI XML error:
  BrainModel requires a child element", although the spec text calls these elements
  optional for the "all vertices/voxels" case (and our reader, like nibabel's parser,
  accepts their absence). The writer therefore always writes the explicit index list, and
  the reader keeps its leniency for files written by others.
- The `.dpconn`/`.pdconn` rows of the file type table in section 4 of this spec were
  **swapped**; the ground truth (fixtures and generator dump) is `.dpconn.nii` =
  (BRAIN_MODELS, PARCELS) with intent 3009 `ConnParcelDense` and `.pdconn.nii` =
  (PARCELS, BRAIN_MODELS) with intent 3010 `ConnDenseParcel`. The section is corrected and
  the writer now refuses a file name that contradicts its axes, so a misnamed file cannot
  be written silently again.
- Verification of the writer is the same loop as for the reader, but in the other
  direction: for all 13 fixtures, a file written from the fixture's axes and data is read
  back by Workbench (`-cifti-convert -to-text`, values equal) and by nibabel (dump
  identical, 368 lines), and files written from scratch (surface only, volume only,
  connectome) are accepted by both as well.
- The user-facing writers write only the structures the data cover: filling the remaining
  grayordinates of a template with NaN would produce a file full of NaN, and the format has
  no missing value. `write.cifti()` on the other hand requires the data to match the axes
  exactly, so the explicit "give me the template mapping" case is still available.
- Naming: the reader's accessor `cifti.axis.labels()` had to be renamed to
  `cifti.dim.labels()` in this increment, because the writer's builder for a LABELS
  dimension takes that name (the builder names now follow the five index types exactly).
  Renamed in the same commit, while the entry is unreleased.
- `cifti.structure.canonical()`/`cifti.structure.short()` are vectorized now; using them
  on a vector of structures in an error message exposed that they were not.

### Increment 9: CIFTI-2 parcellated files and connectomes (2026-09-22) -- DONE (item I.2d)

Files: `R/cifti_parcels.R` (parcels axis from annotations), `R/read_cifti_connectome.R`
(`read.fs.connectome.cifti()`, the `fs.connectome` class and its print method),
`R/write_cifti_connectome.R` (`write.fs.connectome.cifti()`,
`write.fs.parcellated.cifti()`), `tests/testthat/test-cifti_parcels.R` (58 tests),
`tests/testthat/test-cifti_connectome.R` (110 tests), and a third section in
`dev_tools/check_cifti_conversion.R` (written files, the parcel semantic check and the
annotation check). Item I.2c was already complete, so I.2d was the missing half of the
parcellated support; the plan's "pscalar/ptseries/pconn/pdconn/dpconn writers" are covered
by the two new writers.

Public API added: `read.fs.connectome.cifti(filepath, rows, columns)` (plus the
`fs.connectome` class with a print method), `write.fs.connectome.cifti(filepath, data,
template, axes, metadata)`, `write.fs.parcellated.cifti(filepath, data, template, axes,
map_names, start, step, unit, metadata)` and `cifti.axis.parcels.from.annot(annots,
structure, parcel_names, default_label_name)`.

Findings worth keeping:

- **The official example files of the plan do not fit together.** The official
  `Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii` describes the *complete* fs_LR
  surface (32,492 vertices per hemisphere), while the official `.dtseries`/`.ptseries` use
  the reduced grayordinates mapping (30,424 lh, 30,527 rh), so Connectome Workbench
  refuses to combine them: `wb_command -cifti-parcellate <dtseries> <dlabel> COLUMN <out>`
  aborts with "data file is missing vertex 7 in structure CORTEX_LEFT, which is used by
  label 'MEDIAL.WALL'". The parcel lists of the official `.ptseries` are therefore the
  reference for the parcel handling, not the dlabel; the check script builds the parcels
  axis from the official `.ptseries` and reproduces its values, which is a stronger check
  anyway (it is Workbench's own parcellation).
- **A parcel's vertex indices are per brain structure, and the hemispheres overlap
  numerically.** Averaging the values of the vertices of a parcel requires the mapping
  from (structure, vertex index) to a matrix column, i.e. the brainordinate table of the
  file (`cifti.grayordinates()`); `unique(unlist(parcel$vertices))` silently mixes the two
  hemispheres and produces a wrong mean for every parcel that spans both (the first version
  of the check script did exactly that and was off by up to 1.2). Increment 7 got this
  right by taking the vertices per structure; the lesson is that the *axis* alone is not
  enough to index the data.
- `vapply(x, f, numeric(n))` returns an `n` x `length(x)` matrix, i.e. series x parcels,
  which is already the matrix dimension order the writer wants. Adding a `t()` (as the
  first version did) triggers the writer's transpose hint - the hint works.
- **An axis object is a list, so it cannot be passed to `cifti.check.axes()` directly.**
  `cifti.check.axes()` expects a list *of* axes; a single axis (a list of ~9 entries with a
  `type` entry) is reported as "A CIFTI-2 file has at most two matrix dimensions, but 9
  axes were given". The writers therefore detect the single-axis input form by the presence
  of the `type` entry (`is.list(axes) && !is.null(axes$type)`) and wrap it. The same
  pattern is worth using for any future "one axis or a list of axes" parameter.
- **`sprintf()` with a multi-line `paste0()` format needs one argument per placeholder.**
  Two `%s` placeholders and a single pasted argument are an error at *call* time ("too few
  arguments"), which is easy to miss because the message looks unrelated to the format
  string.
- **`identical()` on dimensions needs `unname()`**: `vapply()` over a named list returns a
  named vector, while `dim(matrix)` is unnamed, so `identical(dim(data), dim_sizes)` was
  false for an otherwise matching matrix. The error message it produced was correct
  ("has the dimensions 3 x 3, but its axes describe a matrix of size 3 x 3"), which made
  the cause visible.
- The region names of the two hemispheres are matched *after* removing hemisphere markers
  (`L_`/`R_`/`LH_`/`RH_`/`Left_`/`Right_`, as a prefix, a suffix or an infix), because a
  parcel of a parcellated file is a region that spans the structures it occurs in: a
  Schaefer atlas names the same region `7Networks_LH_Vis_1` and `7Networks_RH_Vis_1`, and
  without the normalization every region would become two parcels. Names without a marker
  are returned unchanged, and a name that consists of nothing but a marker is not touched.
- The parcels of an annotation are ordered like its label table (the atlas order), not like
  the vertices of the mesh: the first vertex of a hemisphere is somewhere in the middle of
  the brain, so a first-appearance order would look arbitrary and would change whenever the
  mesh changes.
- Verification results: 7 files written by the new writers (all four connectome types, the
  two parcellated types and the official 54 parcel `.ptseries`) are read back by Connectome
  Workbench with identical values and by nibabel with an *identical* parsed dump (433
  identical dump lines), a parcellated file built from two FreeSurfer annotations is
  accepted by both, and the official `.ptseries` values are reproduced exactly through our
  writer (max absolute difference 0e+00, i.e. the same float32 numbers). The annotation
  check also verifies that the parcels cover every vertex of the annotation exactly once.
