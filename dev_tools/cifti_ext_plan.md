# Plan: Extend CIFTI support in freesurferformats via the `cifti` package

Status: draft / not started.
Last updated: 2026-09-07.

## Background

`freesurferformats` currently has minimal CIFTI support: a single exported function,
`read.fs.morph.cifti()`, which extracts **surface morphometry data from `*.dscalar.nii`
files** (per-vertex values for `lh`/`rh`/`both`, one data column at a time).

It does this by delegating all NIfTI-2 / CIFTI-extension / XML parsing to the low-level
**`cifti`** package by John Muschelli (a `Suggests` dependency), then rebuilding
FreeSurfer-native structures (per-vertex vectors over a surface). We deliberately chose
`cifti` over `ciftiTools` years ago:

- `cifti` is **read-only, low-level and light** (deps: `xml2`, `oro.nifti`, `gifti`); it just
  parses the CIFTI XML and returns the data matrix plus per-structure metadata.
- `ciftiTools` is a high-level, user-facing toolkit (its own `xifti` object, read/write,
  rgl visualization, Workbench integration) with heavy deps (`rgl`, `RNifti`, ...). It is
  **not** built on `cifti` (it re-implements the parsing itself) and does not fit
  `freesurferformats`' model of returning FreeSurfer-native data structures. Out of scope here.

Goal of this plan: add the **low-hanging CIFTI file types** that are nearly free to support
*through the `cifti` package* — i.e., without writing any NIfTI-2 / extension / XML handling
of our own.

## Key facts about the `cifti` package (verified against CRAN 0.5.0)

`cifti::read_cifti(fname)` returns one generic object regardless of CIFTI intent:

- `$data`: the full data matrix. With default `trans_data=TRUE`, rows = grayordinates (dense
  files) or parcels (parcellated files), columns = maps / time points.
- `$BrainModel`: a list, one entry per brain structure. Each entry carries the structure's
  vertex indices (surface) or voxel IJK indices (volume), plus attributes:
  `ModelType`, `IndexOffset`, `IndexCount`, `SurfaceNumberOfVertices`, `Structure`, ...
- `$Volume`: subcortical volume metadata (dims / origin / spacing).
- `$Surface`, `$Parcel`: surface geometry / parcel definitions (name -> vertices & voxels).
- `$NamedMap`: per-column names (`map_names`); for `dlabel` files also the label table
  (Key, Red, Green, Blue, Alpha, Label).

This is exactly what `read.fs.morph.cifti()` already exploits for the surface part of dscalar
files: it slices `$data` rows by a structure's `IndexOffset`/`IndexCount` and scatters the
values into `SurfaceNumberOfVertices` positions.

Important: because every dense/parcellated intent goes through this same object, the extraction
code barely changes between file types — only the *interpretation of the columns* differs.

### Official test data

`cifti::download_cifti_data(outdir)` downloads the official CIFTI-2 example set, which already
contains all the types below:

- `ones.dscalar.nii`, `Conte69.MyelinAndCorrThickness.32k_fs_LR.dscalar.nii` (dscalar)
- `Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii` (dtseries)
- `Conte69.parcellations_VGD11b.32k_fs_LR.dlabel.nii` (dlabel)
- `Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii` (ptseries)
- `Conte69.L/R.inflated.32k_fs_LR.surf.gii`, `Conte69_AverageT1w_restore.nii.gz`

## Current state (for reference)

- `R/cifti.R`: `read.fs.morph.cifti(filepath, brain_structure, data_column)`.
  Errors on volume model types ("Currently only model type CIFTI_MODEL_TYPE_SURFACE ...").
- `tests/testthat/test-cifti.R`: only dscalar + surfaces, downloads data on the fly,
  CRAN-skipped. Documents a workaround for muschellij2/cifti#9 (CRAN `cifti` version).
- Exported in `NAMESPACE`: `read.fs.morph.cifti`.

## Proposed additions (ordered by effort / value)

### 1. Surface `dlabel` maps (lowest effort, highest value) — RECOMMENDED FIRST

- **What:** read per-vertex integer label keys for `lh`/`rh` from `*.dlabel.nii` (e.g., HCP-MMP,
  Schaefer parcellations on fs_LR — effectively an "HCP-style annot" file).
- **Why cheap:** extraction is a near copy of the dscalar path (same `$BrainModel` slicing);
  only the values are integer label keys instead of floats.
- **Why fits the package:** `freesurferformats` is surface/parcellation-centric (annot, label).
- **New function** (do NOT overload `read.fs.morph.cifti`, since label keys are not
  morphometry): e.g. `read.fs.parcellation.cifti()` returning a per-vertex integer vector for
  one hemisphere (NA for unassigned vertices). Optional `with_label_table`/`with_names` to also
  return the key -> name / RGBA table from `$NamedMap` (could optionally be converted to
  FreeSurfer annot-style color codes later).
- **Open question:** annot-style `data.frame` vs plain integer vector. Decide against the
  existing `read.fs.annot` return conventions.

### 2. Surface `dtseries` (low effort) — OPTIONAL, add if dense-fMRI users wanted

- **What:** read surface time series from `*.dtseries.nii`.
- **Why cheap:** identical plumbing; columns are time points.
- **New function:** e.g. `read.fs.series.cifti()` returning, per hemisphere, a
  (vertices x time) matrix, or a single time column as a morph vector.
- **Open question:** return type (matrix per hemi vs long format); whether to also return the
  volume part (see #3) to be a full grayordinate reader.

### 3. Subcortical / volume part of dense files (low–moderate effort)

- **What:** currently volume model types `stop()` in `read.fs.morph.cifti`. Add support to
  scatter the volume-part values into a 3D array (fs.volume-like) using the structure's
  voxel IJK indices and the `$Volume` dims.
- **Why useful:** unlocks the subcortical part of dscalar/dlabel/dtseries files.
- **Open question:** new function returning an `fs.volume`-like object vs extending the surface
  functions; ordering/axis conventions for the scattered volume.

### 4. Parcellated types `ptseries`/`pscalar` (medium effort) — LATER / only if asked

- `$data` rows are parcels; `$Parcel` gives name -> vertex/voxel membership.
- Option A (trivial): return named per-parcel values (but package has no "parcel" concept yet).
- Option B (more useful, more work): "explode" parcels to per-vertex surface maps by mapping
  parcels -> vertices per hemisphere (turns a parcellated file into a dense surface map).
- Defer until #1–#3 are done and someone actually needs it.

### 5. `dconn` / `pconn` connectivity — SKIP

- NxN over grayordinates (potentially huge); per-surface slices are niche; users reduce these
  with `wb_command`. Not worth the return-type mess in `freesurferformats`.

## Cross-cutting design decisions

- **Refactor first (recommended):** extract a private helper that, given a `cifti` object, a
  structure name and a data slice, returns the per-vertex (or per-voxel) vector for that
  structure. `read.fs.morph.cifti` and all new functions should share it, so structure/offset/
  index logic lives in one place.
- **Keep the `cifti` package as the only CIFTI dependency.** Do not add `ciftiTools` or a
  native NIfTI-2 / XML parser for this.
- **Return types:** each new function should have a clear, documented return type (integer
  label vector, numeric morph vector, verts x time matrix, fs.volume-like array). Prefer new
  functions over bending `read.fs.morph.cifti`.
- **Fragility note:** all of this reaches into `cifti`'s object shape (e.g. attributes on
  `$BrainModel` entries). That coupling already exists; more functions extend it. If `cifti`
  changes shape, tests must catch it (see below) — consider a single internal accessor to
  isolate the assumption.

## Testing plan

- Add tests to `tests/testthat/test-cifti.R` (or a new `test-cifti2.R`) mirroring existing
  patterns:
  - `skip_on_cran()`, skip under `tests_running_on_cran_under_macos()` (data download),
    `skip_if_not_installed("cifti")`, `skip_if_not(file.exists(cii_file))`.
  - Fetch official files via `download_cifti_data()` for dlabel/dtseries/ptseries.
- Assertions:
  - dlabel: length == `SurfaceNumberOfVertices`; compare to expected values / count of
    non-NA; round values against the Conte69 reference if stable.
  - dtseries: dims == (n_verts, n_time); a known probe value.
  - volume part (#3): dims match `$Volume` dims; probe a known voxel if available.
- Run locally with `devtools::test()`; keep everything CRAN-safe (downloads only outside CRAN).

## Out of scope

- Writing CIFTI files.
- `ciftiTools` integration / Workbench (`wb_command`) wrappers.
- Native (dependency-free) NIfTI-2 / CIFTI XML parsing in this package.
- Rust / Julia / Java ports (tracked separately in their own repos).

## Status checklist

- [ ] Refactor shared structure-extraction helper in `R/cifti.R`
- [ ] #1 Surface `dlabel` reader + tests
- [ ] (optional) #2 Surface `dtseries` reader + tests
- [ ] (optional) #3 Subcortical volume-part support + tests
- [ ] (later) #4 Parcellated types
- [ ] Update `CHANGES` ("Current WIP")
