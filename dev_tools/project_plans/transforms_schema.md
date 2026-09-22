# Transforms: object schema and increment plan

Replaces/refines item I.4 of `plan.md`. Written 2026-09-22.

## Scope (agreed)

**In:** one transform object + object schema; FSL `.mat` read/write; then writers for the
three FreeSurfer formats we already read (`lta`, `dat` = register.dat, `xfm`); explicit,
documented coordinate-space handling.

**Out:** ITK `.tfm` / ITK binary `.mat` (deferred, separate increment later), any HDF5/`.h5`,
resampling/`applyxfm` functionality, nonlinear/composite warps.

Guiding rule: the package must be able to *write* what it can *read*, and a reader must never
guess or silently convert conventions. Both are usability properties, not format count.

## 1. The object

Class `c("fs.transform", "list")`.

| field | type | meaning / invariants |
| --- | --- | --- |
| `matrix` | 4x4 numeric | **Always** maps src -> dst: `dst_h = matrix %*% c(src, 1)`. Same convention as `doapply.transform.mtx()`. Never stores an inverse. Always present. |
| `space_in` | character | `"voxel"`, `"ras"` or `"lps"` - the *kind* of coordinate the matrix consumes. |
| `space_out` | character | same vocabulary, for the output coordinates. |
| `voxel_base` | integer 0/1, or `NA` | Only meaningful when a space is `"voxel"`. `0` = NIfTI/FSL/ITK (voxel (0,0,0) is the first voxel centre); `1` = FreeSurfer/tkregister (voxel (1,1,1) is the first voxel). Prevents off-by-one-voxel errors. |
| `src` | `NULL` or list | Describes the volume/template the matrix maps **from**. A partial descriptor is allowed; `NULL` means "the file does not say". See below. |
| `dst` | `NULL` or list | Same, for the target. |
| `format` | character | `"fslmat"`, `"lta"`, `"dat"`, `"xfm"`. Identifies the reader/writer that produced it. |
| `source` | `NULL` or character | File path the transform was read from. |
| `type` | as today | Format-native type string, unchanged from the current readers (`"Linear"` for xfm, LTA type, ...). Do **not** overload this for our own tagging; `format` is the package-level tag. |
| *format extras* | as today | `$header` + `$volumes` (LTA), `$intensity` (dat). Retained verbatim so existing code/tests keep working. |

Volume descriptor (used for `src`/`dst`), all fields optional except as noted:

| field | meaning |
| --- | --- |
| `path` | file name/path recorded in the transform file, if any |
| `dim` | integer vector of 3 (or 4) volume dimensions |
| `voxelsize` | numeric vector of 3 voxel sizes in mm |
| `vox2ras` | 4x4 matrix, voxel -> RAS, **in the frame given by `frame` and with the zero-based voxel indices that `mghheader.vox2ras()` uses** |
| `frame` | `"scanner"` (the volume's own RAS, as recorded in the file) or `"tkreg"` (FreeSurfer tkregister RAS). Default `"scanner"`. |

Rationale for splitting `space_*` and `frame`: the coordinate *type* (voxel vs RAS vs LPS) is a
small, closed vocabulary and belongs to the matrix; *whose* RAS it is belongs to the endpoint.

## 2. Per-format mapping

| format | `matrix` maps | `space_in` | `space_out` | `voxel_base` | `src`/`dst` from file? | extras |
| --- | --- | --- | --- | --- | --- | --- |
| `fslmat` (FSL/FLIRT `.mat`) | voxels of the `-in` image -> voxels of the `-ref` image | `"voxel"` | `"voxel"` | 0 | **No** - the two images are not recorded, the caller must supply them | - |
| `lta` | per header `type`: `0` = VOX2VOX, `1` = RAS2RAS | from `type` | from `type` | 0 (type 0) | **Yes** - `filename`, `volume`, `voxelsize`, `xras`/`yras`/`zras`/`cras` per volume, `valid` | `$header`, `$volumes` (incl. `mean`, `nxforms`) |
| `xfm` (talairach.xfm) | subject RAS -> target (MNI/Talairach) RAS | `"ras"` | `"ras"` | `NA` | No (not recorded) | `$type` (`"Linear"`) |
| `dat` (tkregister / register.dat) | voxels of the **movable** volume -> **tkreg RAS** of the **target** | `"voxel"` | `"ras"` | 0 | No - only the subject id is in the file (target -> `dst$frame = "tkreg"`) | `$intensity` |

Notes / confirmations:

- `talairach.lta` in `inst/extdata` has `type = 0 # LINEAR_VOX_TO_VOX`, and its `dst` volume is
  the MNI305 template (`.gca`) with `cras = 0 0 0` and a signed-permutation `xras`/`yras`/`zras`:
  a textbook voxel-to-voxel LTA whose endpoint frames differ, i.e. exactly why `matrix` alone is
  not enough information.
- LTA `type` semantics confirmed against FreeSurfer's `lta_convert`: RAS2RAS is the default
  output type, VOX2VOX is requested explicitly via `--ltavox2vox`. Any other `type` value ->
  `space_in`/`space_out` = `NA` **with a warning**, never a guess.
- register.dat direction confirmed by `lta_convert --help`: "For TKREG/register.dat type
  matrices, src=mov and trg=ref/targ".
- **FSL `.mat` is voxel-to-voxel, not a world affine.** Independently confirmed: an identity
  `.mat` run through `transformconvert flirt_import` with two real T1w images yields a non-identity
  world matrix. Any API treating `.mat` like our LTA/xfm matrices would be silently wrong.

### Volume geometry conventions (verified against FreeSurfer 7.4.1)

An earlier draft of this document assumed one-based voxel indices for the FreeSurfer formats and
treated `cras` as the translation column of the voxel-to-RAS matrix. **Both were wrong**, and the
error is half the field of view, so it is recorded here for good. Measured on a real `nu.mgz`
(256x256x256, 1 mm) with the LTA written by `recon-all`:

| quantity | value | source of truth |
| --- | --- | --- |
| `cras` | `(-0.5, 29.37, -48.90)` | `mri_info --cras`, and the `cras` of the LTA's `src` volume info - identical |
| voxel-to-RAS (scanner) | translation `(127.50005, -98.62723, 79.09527)` | `mri_info --vox2ras`, and `mghheader.vox2ras()` - identical |
| voxel-to-RAS (tkreg) | translation `(128, -128, 128)` | `mri_info --vox2ras-tkr` |
| `vox2ras_tkr` vs `vox2ras` | differ by exactly `cras` | `mghheader.tkreg2scanner()` is a pure translation |

Consequences:

- `cras` is the RAS coordinate of the **centre** of the volume, i.e. of voxel index `dim/2`.
- The voxel-to-RAS matrix is `Mdc_scaled = [xras yras zras]`, `Pxyz_0 = cras - Mdc_scaled * (dim/2)`,
  which is exactly what `mghheader.vox2ras()` implements for MGH headers. Verified: the matrix built
  from the LTA volume info differs from `mghheader.vox2ras()` by `(dim/2)*(direction)` only if
  `cras` is misused, and by `0` when built correctly.
- FreeSurfer voxel indices are **zero-based** (index 0 is the first voxel, the volume centre is at
  `dim/2`), so `voxel_base = 0` for `lta`, `dat` and `fslmat`. Note the further subtlety that
  `vox2ras` maps an index to the voxel's lower corner, not its centre; this is inherited from
  `mghheader.vox2ras()` and not changed here.
- `volume.descriptor()` therefore needs `dim` to build `vox2ras`; the direction vectors alone are not
  enough, and it warns rather than silently dropping the geometry.

## 3. Conversion is explicit, never silent

- Readers return exactly what the file says, tagged with spaces. They never convert.
- `transform.to.world(tf, src = NULL, dst = NULL)` / `transform.to.voxel(...)`: convert
  between voxel and RAS using the volume geometry. Error (not a warning, not a guess) when the
  required geometry is missing, naming what to pass.
- `invert.fs.transform(tf)`: swaps `src`/`dst` and inverts `matrix`; spaces swap accordingly.
- `write.fs.transform()` validates: writing `format = "fslmat"` requires
  `space_in == space_out == "voxel"` and `voxel_base == 0`, otherwise it errors with a hint to
  call `transform.to.voxel()` first. Same "refuse rather than guess" policy for the other writers
  (e.g. an RAS2RAS LTA written as an xfm is fine; a voxel-space matrix is not).

## 4. API surface

- `read.fs.transform(filepath, format = "auto")` - existing entry point, extended with
  `"fslmat"`. `"auto"` uses the extension plus a content sniff.
- `write.fs.transform(tf, filepath, format = "auto")` plus the per-format functions.
- `print.fs.transform()` / `summary.fs.transform()`: format, direction (src -> dst), spaces,
  voxel base, determinant, translation norm; warnings for a non-invertible matrix, a 4th row
  other than `(0,0,0,1)`, or `NA` spaces.
- `is.fs.transform()`; internal constructor/validator `fs.transform(...)` as the single place
  that checks invariants (used by every reader).
- Naming: keep a **single** entry point. Do not introduce `read.fsl.transform()` next to
  `read.fs.transform()` - "fsl" vs "fs" is a coin flip to read and a support burden.

## 5. Backward compatibility and known pitfalls

- Keep `$matrix`, `$type`, `$volumes`, `$header`, `$intensity` exactly as they are; the new
  fields are additive. Existing tests and callers keep working.
- **`.mat` extension clash**: FSL writes text, ANTs/ITK writes binary/MATLAB-v4 under the same
  extension. `format = "auto"` must sniff content; when it detects an ITK transform
  (`#Insight`, `AffineTransform_`, `Euler3DTransform_`) it must fail with a message that names
  ANTs/ITK explicitly instead of a parse error.
- **Voxel base**: FreeSurfer/tkregister voxel coordinates are 1-based, FSL/ITK/NIfTI are
  0-based. `voxel_base` records it; never assume.
- **FSL flip depends on the image type**: `lta_convert --outfsl` documents that
  `FSLOUTPUTTYPE` (NIFTI vs ANALYZE) determines the flipping. Document it; the FSL helpers must
  take the volume geometry, not just the matrix.
- **Existing bug**: `read.fs.transform.dat()` uses `as.integer()` for the intensity line, so the
  shipped `register.dat` value `0.150000` comes back as `0`. One-line fix to `as.numeric()` plus
  a CHANGES entry - worth folding in while the schema work touches this file.

## 6. Validation (dev_tools/, never package tests)

Two independent implementations are available locally, which removes all guesswork about
conventions:

| oracle | path | use |
| --- | --- | --- |
| FreeSurfer 7.4.1 `lta_convert` | `~/software/freesurfer/freesurfer7.4.1/bin/lta_convert` | reads `--inlta/--inxfm/--intkreg/--infsl`, writes `--outlta/--outfsl/--outxfm/--outtkreg`, `--ltavox2vox`; converts between all four formats -> can **generate** genuine FSL `.mat` files from known LTAs, no FSL install needed |
| MRtrix3 3.0.8 `transformconvert` | `~/software/micromamba/envs/mrtrix3/bin/transformconvert` | `flirt_import` (`mat + in image + ref image`) validates the FSL voxel-space convention; `itk_import` bit-exactly reproduces QSIRECON's ITK->MRtrix output (verified) |

Strategy: golden files for package tests, and a `dev_tools/` script that round-trips our output
through both oracles. Package tests must stay self-contained and skip when `extra_test_data` is
absent (see `find_extra_test_data_file()`), so no MRtrix/FreeSurfer dependency is ever added.

## 7. Increments

1. **DONE (2026-09-22)** - schema + plumbing: `R/transforms_common.R` with the constructor
   `fs.transform()`, the validator `validate.fs.transform()`, the volume descriptor helper
   `volume.descriptor()`, and `is.fs.transform()`, `invert.fs.transform()`, `print.fs.transform()`,
   `summary.fs.transform()`. The three FreeSurfer readers fill the new fields, the register.dat
   intensity bug is fixed, and `read.fs.transform.xfm()`/`.lta()` now report a missing 4x4 matrix
   instead of returning a `NULL` one. Tests in `tests/testthat/test-transforms_common.R`.
2. **FSL `.mat` read + write**, `transform.to.world()`/`transform.to.voxel()`, `.mat` sniffing.
   `transform.to.world()` accepts `fs.volume` and `nifti` instances. Tests + validation against both
   oracles on real data.
3. **Writers** for `lta`, `dat`, `xfm` (+ `write.fs.transform` dispatch, round-trip tests,
   validation that FreeSurfer itself can read what we write).
4. *(later, separate)* ITK `.tfm` text, then ITK binary `.mat`.

## 8. Open decisions

- `space_in`/`space_out` vocabulary: `"voxel" | "ras" | "lps"` + `src/dst$frame`, or
  `"voxel" | "scanner_ras" | "tkreg_ras" | "lps"`? (Recommendation: the former - the frame
  belongs to the endpoint, and it keeps the closed vocabulary small.)
- `transform.to.world()`: needs `src`/`dst` vox2ras. For FSL `.mat` the caller must supply both
  images; accept full volume objects (`fs.volume`, NIfTI) or header-only lists?
