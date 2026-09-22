#!/usr/bin/env python3
"""Generate test data for the ANALYZE 7.5 reader/writer of freesurferformats.

Writes tiny two-file image fixtures, in both variants of the 348 byte header: plain ANALYZE 7.5 (empty magic,
the format that 3D Slicer and the old tools write) and NIFTI v1 pairs ('ni1' magic, what FSL writes). Also
writes reference dumps of the values (read back with the reference implementation of both formats, nibabel) and
of the geometry fields, which dev_tools/check_analyze_conversion.R compares against our reader.

Usage:
    python3 dev_tools/generate_analyze_test_data.py generate <out_dir>

The fixtures are tiny on purpose: the point is the variety of header variants, not the size. Fixtures that are
small enough to be shipped with the package go to <out_dir>/small (copy them to inst/extdata/analyze in the
repo), the rest (and all reference dumps) go to <out_dir> (copy everything to extra_test_data/analyze). The
developer check script and the dev-only tests use the latter. Note that both copies have to be updated after
regenerating, the unit tests read the shipped one.

Requires nibabel. Writes no file that was produced by a third party tool, so the fixtures can be redistributed
with the package.
"""
import os
import sys
import numpy as np
import nibabel as nib
import scipy.io as sio
from nibabel.analyze import AnalyzeHeader, AnalyzeImage
from nibabel.nifti1 import Nifti1Image
from nibabel.spm99analyze import Spm99AnalyzeImage

# The world transform that the NIFTI pair fixtures store. It is a rotation with a flip and a non-trivial
# translation, so that a reader that ignores the sform, or that computes it wrongly, is caught.
PAIR_AFFINE = np.array([
    [-1., 0., 0., 10.],
    [0., 0., 1., -20.],
    [0., -1., 0., 30.],
    [0., 0., 0., 1.],
])

# A second one, with non-unit voxel sizes, for the compressed pair.
PAIR_AFFINE_ZOOMED = np.array([
    [0., 0., -2.5, 5.],
    [1.5, 0., 0., -6.],
    [0., 1.5, 0., 7.],
    [0., 0., 0., 1.],
])

# 4x3x2 uint8, values 1..24 in file order (the first axis varies fastest, which is the order both formats use).
VOL_U8 = np.arange(1, 25, dtype=np.uint8).reshape((4, 3, 2), order='F')

# 4x3x2 int16 with negative values and values above 127 (to catch a reader that uses the wrong signedness).
VOL_I16 = np.array([
    -1, 130, 3, 32767, -32768, 6, 7, 8, 9, 1000, -1000, 12,
    13, 14, -250, 16, 17, 200, 19, 20, 21, 22, 23, 24], dtype=np.int16).reshape((4, 3, 2), order='F')

# 4x3x2x4 int16 time series: frame f contains the values 1..24 plus 100*f, so the values also identify the frame.
# The frames are stacked in the last axis, which keeps the 24 values of one frame contiguous in the file.
VOL_4D = np.stack([(np.arange(1, 25) + 100 * frame).reshape((4, 3, 2), order='F') for frame in range(4)], axis=3).astype(np.int16)

# 4x3x2 uint16, with values above 32767 to catch a reader that reads unsigned data as signed. The values are
# listed explicitly here so that the tests can state them, since the type cannot store all of the values of the
# int16 fixture above plus an offset.
VOL_U16 = np.array([
    40000, 65535, 1, 50000, 32768, 6, 7, 8, 9, 1000, 60000, 12,
    13, 14, 33000, 16, 17, 200, 65534, 20, 21, 22, 23, 24], dtype=np.uint16).reshape((4, 3, 2), order='F')

EXPECTED_KEYS_INT = ["sizeof_hdr", "datatype", "bitpix", "views", "vols_added", "smin", "glmax", "glmin", "dim_un0", "unused1", "extents"]
EXPECTED_KEYS_FLOAT = ["vox_offset", "funused1", "funused2", "funused3", "cal_min", "cal_max"]
EXPECTED_KEYS_BYTE = ["orient"]
EXPECTED_KEYS_STR = ["descrip", "originator", "data_type", "db_name", "vox_units", "cal_units", "aux_file", "regular", "hkey_un0"]
EXPECTED_KEYS_NIFTI1_INT = ["sizeof_hdr", "datatype", "bitpix", "qform_code", "sform_code", "xyzt_units", "slice_code", "glmax", "glmin"]
EXPECTED_KEYS_NIFTI1_FLOAT = ["scl_slope", "scl_inter", "quatern_b", "quatern_c", "quatern_d", "qoffset_x", "qoffset_y", "qoffset_z", "cal_min", "cal_max", "vox_offset"]
EXPECTED_KEYS_NIFTI1_STR = ["descrip", "aux_file", "intent_name", "magic"]


def write_analyze(outdir, basename, data, pix_dim, orient=0, funused1=0.0, funused2=0.0, spm_origin=None,
                  descrip="freesurferformats test data", affine=None, gzipped=False, patch_bitpix=None):
    """Write one plain ANALYZE 7.5 fixture and its expected values dump. Returns the header fields we set."""
    shape = data.shape
    header = AnalyzeHeader()
    header.set_data_dtype(data.dtype)
    header.set_data_shape(shape)
    header.set_zooms(tuple(pix_dim[:len(shape)]))
    header['pixdim'][1:4] = pix_dim[:3]
    header['orient'] = bytes([orient])
    header['funused1'] = funused1
    header['funused2'] = funused2
    header['descrip'] = descrip.encode('ascii')
    if spm_origin is not None:
        header['originator'] = b''.join(int(o).to_bytes(2, 'little', signed=True) for o in spm_origin)

    # ANALYZE stores no affine, but nibabel derives the pixdim field from the affine it is given when saving, so
    # the affine has to describe the voxel sizes we want the file to have.
    if affine is None:
        affine = np.diag([pix_dim[0], pix_dim[1], pix_dim[2], 1.0])
    img = AnalyzeImage(data, affine, header)
    suffix = '.hdr.gz' if gzipped else '.hdr'
    fixture = os.path.join(outdir, basename + suffix)
    nib.save(img, fixture)

    # Some old writers stored a wrong value in the 'bitpix' field. Patch it here, before dumping, so that the
    # dump describes the file as it is on disk (a reader has to trust the 'datatype' field and warn).
    if patch_bitpix is not None:
        with open(fixture, 'r+b') as f:
            f.seek(72)  # the bitpix field
            f.write(int(patch_bitpix).to_bytes(2, 'little'))

    # Read the fixture back with nibabel, with the plain ANALYZE reader (no scaling, no SPM origin) and with the
    # SPM reader (scale factor from funused1, origin from the originator field), to get both reference geometries.
    reread = nib.load(fixture)
    plain = nib.analyze.AnalyzeImage.from_filename(fixture)
    spm = nib.spm99analyze.Spm99AnalyzeImage.from_filename(fixture)
    dump = {}
    dump['values'] = np.asarray(plain.dataobj.get_unscaled()).ravel(order='F')
    dump['nibabel_class'] = type(reread).__name__
    if patch_bitpix is not None:
        # nibabel repairs the wrong bitpix field in its in-memory header, so the value it reports below is not the
        # one that is stored in the file. The checker compares our reader against the file value, known here.
        dump['file_bitpix'] = int(patch_bitpix)
    dump['nibabel_affine'] = plain.affine
    dump['nibabel_spm_affine'] = spm.affine
    dump['nibabel_orient_byte'] = int.from_bytes(bytes(AnalyzeHeader.from_fileobj(header_fileobj(fixture))['orient']), 'little')
    dump['spm_origin'] = spm_origin
    write_dump(outdir, basename + suffix, AnalyzeHeader.from_fileobj(header_fileobj(fixture)), dump, 'analyze')

    # Self-check: the file we just wrote has to contain the values and voxel sizes we intended.
    if not np.array_equal(dump['values'], data.ravel(order='F')):
        raise RuntimeError("fixture '%s' does not contain the data that was passed in" % fixture)
    written_pixdim = [float(v) for v in nib.analyze.AnalyzeHeader.from_fileobj(header_fileobj(fixture))['pixdim'][1:4]]
    if written_pixdim != [float(v) for v in pix_dim[:3]]:
        raise RuntimeError("fixture '%s' has pixdim %s instead of %s" % (fixture, written_pixdim, list(pix_dim[:3])))
    return dump


def write_nifti_pair(outdir, basename, data, affine, gzipped=False, qform_only=False):
    """Write one NIFTI v1 pair fixture ('ni1' magic) and its expected values dump."""
    img = Nifti1Image(data, affine)
    img.header.set_data_dtype(data.dtype)
    if qform_only:
        img.set_sform(None)
        img.set_qform(affine, code=1)
    suffix = '.hdr.gz' if gzipped else '.hdr'
    fixture = os.path.join(outdir, basename + suffix)
    nib.save(img, fixture)

    reread = nib.load(fixture)
    dump = {}
    dump['values'] = np.asarray(reread.dataobj.get_unscaled()).ravel(order='F')
    dump['nibabel_class'] = type(reread).__name__
    dump['nibabel_affine'] = reread.affine
    dump['magic'] = bytes(reread.header['magic'])
    dump['vox_offset'] = float(reread.header['vox_offset'])
    dump['sform_code'] = int(reread.header['sform_code'])
    dump['qform_code'] = int(reread.header['qform_code'])
    write_dump(outdir, basename + suffix, nib.nifti1.Nifti1PairHeader.from_fileobj(header_fileobj(fixture)), dump, 'nifti1')

    # Self-check: the file we just wrote has to contain the values we intended and the affine we asked for.
    if not np.array_equal(dump['values'], data.ravel(order='F')):
        raise RuntimeError("fixture '%s' does not contain the data that was passed in" % fixture)
    if not np.allclose(np.asarray(dump['nibabel_affine']), affine):
        raise RuntimeError("fixture '%s' does not store the affine that was passed in" % fixture)
    return dump


def header_fileobj(path):
    """Open a fixture header file for binary reading, which may be gzipped (the extension is '.hdr.gz' then)."""
    if path.endswith('.gz'):
        import gzip
        return gzip.open(path, 'rb')
    return open(path, 'rb')


def write_spm_analyze(outdir, basename, data, affine):
    """Write an ANALYZE file in the SPM variant, i.e. with the MATLAB sidecar file that holds the matrix.

    nibabel writes both variables that such a file may contain ('mat', which includes the flip of the first axis,
    and 'M', which does not), and FreeSurfer writes only 'M'. The affine is stored in the sidecar, and the read-back
    affine is the reference for the reader of freesurferformats.
    """
    img = Spm99AnalyzeImage(data, affine)
    img.header.set_data_dtype(data.dtype)
    img.header.set_zooms((1., 1., 1.))
    fixture = os.path.join(outdir, basename + '.hdr')
    nib.save(img, fixture)

    with open(os.path.join(outdir, basename + '.mat'), 'rb') as f:
        sidecar_variables = sorted(k for k in sio.loadmat(os.path.join(outdir, basename + '.mat')) if not k.startswith('__'))

    reread = nib.load(fixture)
    plain = nib.analyze.AnalyzeImage.from_filename(fixture)
    dump = {}
    dump['values'] = np.asarray(plain.dataobj.get_unscaled()).ravel(order='F')
    dump['nibabel_class'] = type(reread).__name__
    dump['nibabel_affine'] = reread.affine
    dump['mat_sidecar'] = ' '.join(sidecar_variables)
    dump['spm_origin'] = None
    write_dump(outdir, basename + '.hdr', AnalyzeHeader.from_fileobj(header_fileobj(fixture)), dump, 'analyze')

    if not np.array_equal(dump['values'], data.ravel(order='F')):
        raise RuntimeError("fixture '%s' does not contain the data that was passed in" % fixture)
    if not np.allclose(np.asarray(dump['nibabel_affine']), affine):
        raise RuntimeError("fixture '%s': nibabel does not read back the affine that was stored" % fixture)
    return dump


def write_dump(outdir, fixture_name, header, dump, header_format='analyze'):
    """Write the reference dump for one fixture, with values, header fields and geometry."""
    expected_dir = os.path.join(outdir, 'expected')
    os.makedirs(expected_dir, exist_ok=True)
    lines = []
    lines.append("header_format %s" % header_format)
    if header_format == 'analyze':
        int_keys, float_keys, byte_keys, str_keys = (EXPECTED_KEYS_INT, EXPECTED_KEYS_FLOAT, EXPECTED_KEYS_BYTE, EXPECTED_KEYS_STR)
    else:
        int_keys, float_keys, byte_keys, str_keys = (EXPECTED_KEYS_NIFTI1_INT, EXPECTED_KEYS_NIFTI1_FLOAT, [], EXPECTED_KEYS_NIFTI1_STR)
    for key in int_keys:
        lines.append("header_int %s %d" % (key, int(header[key])))
    for key in float_keys:
        lines.append("header_float %s %s" % (key, repr(float(header[key]))))
    for key in byte_keys:
        lines.append("header_byte %s %d" % (key, bytes(header[key])[0]))
    for key in ['dim']:
        lines.append("header_intlist %s %s" % (key, ' '.join(str(int(v)) for v in header[key])))
    lines.append("header_floatlist pix_dim %s" % ' '.join(repr(float(v)) for v in header['pixdim']))
    if header_format == 'nifti1':
        for key in ['srow_x', 'srow_y', 'srow_z']:
            lines.append("header_floatlist %s %s" % (key, ' '.join(repr(float(v)) for v in header[key])))
    for key in str_keys:
        raw = bytes(header[key])
        printable = ''.join(chr(b) if 32 <= b < 127 else '.' for b in raw).rstrip('.')
        lines.append("header_str %s %s" % (key, printable))
        lines.append("header_hex %s %s" % (key, raw.hex()))
    for key, value in dump.items():
        if key in ('values', 'nibabel_affine', 'nibabel_spm_affine'):
            continue
        if value is None:
            lines.append("dump %s none" % key)
        elif isinstance(value, bytes):
            lines.append("dump %s %s" % (key, value.decode('latin-1').replace('\x00', '.')))
        elif isinstance(value, str):
            lines.append("dump %s %s" % (key, value))
        elif isinstance(value, (list, tuple, np.ndarray)):
            lines.append("dump %s %s" % (key, ' '.join(("%d" % v) if isinstance(v, (int, np.integer)) else repr(float(v)) for v in value)))
        else:
            lines.append("dump %s %s" % (key, ("%d" % value) if isinstance(value, (int, np.integer)) else repr(float(value))))
    lines.append("dump num_values %d" % len(dump['values']))
    values = ['%d' % v if float(v).is_integer() else repr(float(v)) for v in dump['values']]
    lines.append("values %s" % ' '.join(values))
    lines.append("dump nibabel_affine %s" % ' '.join(repr(float(v)) for v in np.asarray(dump['nibabel_affine']).ravel()))
    if 'nibabel_spm_affine' in dump:
        lines.append("dump nibabel_spm_affine %s" % ' '.join(repr(float(v)) for v in np.asarray(dump['nibabel_spm_affine']).ravel()))
    with open(os.path.join(expected_dir, fixture_name + '.expected.txt'), 'w') as f:
        f.write('\n'.join(lines) + '\n')


def generate(out_dir):
    small_dir = os.path.join(out_dir, 'small')
    os.makedirs(small_dir, exist_ok=True)
    os.makedirs(out_dir, exist_ok=True)

    # --- Fixtures small enough to ship with the package. ---

    # Plain ANALYZE 7.5, the basic case: uint8, non-unit voxel sizes, non-zero orient code.
    write_analyze(small_dir, 'tiny_u8', VOL_U8, pix_dim=(1., 2., 3.), orient=1,
                  descrip="freesurferformats")
    # Plain ANALYZE 7.5 with a signed 16 bit type and negative values.
    write_analyze(small_dir, 'tiny_i16', VOL_I16, pix_dim=(1., 1., 1.), orient=2)
    # Plain ANALYZE 7.5 with the fields that SPM re-uses: funused1 as the data scale factor, and the image
    # origin stored in the first 6 bytes of the originator field.
    write_analyze(small_dir, 'tiny_spm', VOL_U8, pix_dim=(1., 1., 1.), funused1=0.5, spm_origin=(2, 2, 2))
    # A 4D ANALYZE file (a time series): dim[0] is 4, the pix sizes of the axes are still the first three.
    write_analyze(small_dir, 'tiny_4d', VOL_4D, pix_dim=(1., 2., 2., 1.), orient=0)
    # A NIFTI v1 pair with a proper sform.
    write_nifti_pair(small_dir, 'pair_u8', VOL_U8, PAIR_AFFINE)
    # An ANALYZE file of the SPM variant, i.e. with the MATLAB sidecar file that stores the transformation matrix.
    write_spm_analyze(small_dir, 'spm_mat_u8', VOL_U8, PAIR_AFFINE)
    # A NIFTI v1 pair that only has a qform, so the quaternion code path is covered.
    write_nifti_pair(small_dir, 'pair_qform_i16', VOL_I16, PAIR_AFFINE_ZOOMED, qform_only=True)
    # Compressed pairs, for both variants.
    write_analyze(small_dir, 'tiny_u8_gz', VOL_U8, pix_dim=(1., 2., 3.), gzipped=True)
    write_nifti_pair(small_dir, 'pair_u8_gz', VOL_U8, PAIR_AFFINE_ZOOMED, gzipped=True)

    # --- Files that only need to exist in the dev checkout (bigger, or not needed for the unit tests). ---
    # A uint16 NIFTI pair: ANALYZE 7.5 has no unsigned 16 bit type (that is an addition of NIFTI), so this can
    # only be tested with the pair variant, which uses the NIFTI data type codes.
    write_nifti_pair(out_dir, 'vol_u16_pair', VOL_U16, PAIR_AFFINE)
    # A float32 ANALYZE file.
    write_analyze(out_dir, 'vol_f32', VOL_U8.astype(np.float32) / 4., pix_dim=(1., 1., 2.))
    # An ANALYZE file whose 'bitpix' field contradicts its 'datatype' field, which old writers produced: the file
    # is an int16 file, but the bitpix field claims 32 bits per value. Readers have to trust the data type.
    write_analyze(out_dir, 'vol_badbitpix', VOL_I16, pix_dim=(1., 1., 1.), patch_bitpix=32)

    return out_dir


def main():
    if len(sys.argv) != 3 or sys.argv[1] != 'generate':
        print(__doc__)
        return 1
    out_dir = sys.argv[2].rstrip('/')
    generate(out_dir)
    print("Wrote ANALYZE test data to '%s' (shippable fixtures in 'small')." % out_dir)
    return 0


if __name__ == '__main__':
    sys.exit(main())
