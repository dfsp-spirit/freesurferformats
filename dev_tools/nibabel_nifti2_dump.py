#!/usr/bin/env python3
"""Print a compact dump of a NIFTI v2 file (and of its header extensions), read with nibabel.

Used by dev_tools/check_nifti2_extensions.R to verify the files that freesurferformats writes with an independent
implementation: the R script writes a file, calls this script on it and compares the output. The script can also
create a reference file itself ("write_reference"), so that the R reader can be tested against a file that only
nibabel has touched.

Usage:
    python3 dev_tools/nibabel_nifti2_dump.py dump <file.nii|file.nii.gz> [<extension-payload-dir>]
    python3 dev_tools/nibabel_nifti2_dump.py write_reference <out.nii> <out.nii.gz>

The 'dump' action prints lines of the form 'key value', one per line, plus one line per header extension:
'extension <index> <ecode> <size_on_disk> <content_length>'. If a payload directory is given, the payload of each
extension is written to '<dir>/<index>.bin' (without the trailing NUL padding), so that the R side can compare
the bytes exactly. The header fields and the extension sizes are read from the file bytes, since nibabel repairs
some header fields while loading. Requires nibabel.
"""
import gzip
import os
import struct
import sys

import numpy as np

import nibabel as nib


def open_plain(path, mode='rb'):
    """Open a file, transparently decompressing it if it is gzipped."""
    if path.endswith('.gz'):
        return gzip.open(path, mode)
    return open(path, mode)


def read_extension_area(path):
    """Read the sizes and codes of the header extensions straight from the file bytes.

    This is independent of nibabel, so that a file whose payload nibabel does not interpret (e.g. the CIFTI XML)
    is still described correctly. Returns a list of (size_on_disk, ecode) tuples.
    """
    with open_plain(path) as fh:
        header = fh.read(544)
        vox_offset = struct.unpack('<q', header[168:176])[0]
        if header[540] == 0:
            return []
        rest = fh.read(max(0, vox_offset - 544))
    extensions = []
    available = len(rest)
    pos = 0
    while available >= 16:
        size, ecode = struct.unpack('<ii', rest[pos:pos + 8])
        if size == 0:
            break
        extensions.append((size, ecode))
        pos += size
        available -= size
    return extensions


def dump(path, payload_dir=None):
    img = nib.load(path)
    # CIFTI-2 files are NIFTI-2 files, but nibabel gives them a CIFTI header, so the NIFTI header is taken from
    # the 'nifti_header' attribute if it exists (both image classes have it).
    hdr = getattr(img, 'nifti_header', None) or img.header
    data = np.asarray(img.dataobj)
    print("num_values %d" % data.size)
    print("shape %s" % ' '.join(str(v) for v in data.shape))
    print("sum %.10g" % float(data.sum()))
    print("dtype %s" % (img.get_data_dtype() if hasattr(img, 'get_data_dtype') else 'unknown'))
    print("dim %s" % ' '.join(str(int(v)) for v in hdr['dim']))
    print("datatype %d" % int(hdr['datatype']))
    print("bitpix %d" % int(hdr['bitpix']))
    print("intent_code %d" % int(hdr['intent_code']))
    # Some header fields are repaired while loading (nibabel sets a zero pixdim to 1), so they are also reported
    # as they are stored in the file.
    with open_plain(path) as fh:
        raw = fh.read(544)
    print("raw_magic %s" % raw[4:12].hex())
    print("raw_extension_flag %s" % raw[540:544].hex())
    print("raw_vox_offset %d" % struct.unpack('<q', raw[168:176])[0])
    print("raw_pixdim %s" % ' '.join(repr(float(v)) for v in struct.unpack('<8d', raw[104:168])))

    file_extensions = read_extension_area(path)
    payloads = list(hdr.extensions)
    print("num_extensions %d" % len(payloads))
    for index, ext in enumerate(payloads):
        # nibabel keeps the raw payload of an extension in '_raw' if it knows how to interpret the code (the
        # CIFTI extension is turned into a header object instead). Trailing NUL bytes were removed by nibabel.
        content = getattr(ext, '_raw', None)
        if not isinstance(content, (bytes, bytearray)):
            content = bytes(ext.get_content())
        content = bytes(content)
        size = file_extensions[index][0] if index < len(file_extensions) else (len(content) + 23) // 16 * 16
        print("extension %d %d %d %d" % (index, int(ext.get_code()), int(size), len(content)))
        if payload_dir is not None:
            os.makedirs(payload_dir, exist_ok=True)
            with open(os.path.join(payload_dir, "%d.bin" % index), 'wb') as fh:
                fh.write(content)
    return 0


def write_reference(path, gzpath):
    """Write a NIFTI v2 file with two header extensions and known data, using nibabel."""
    from nibabel.nifti1 import Nifti1Extension
    from nibabel.nifti2 import Nifti2Image

    data = np.arange(1, 25, dtype=np.float32).reshape((4, 3, 2), order='F')
    img = Nifti2Image(data, np.eye(4))
    img.header.extensions.append(Nifti1Extension(6, b'a reference comment extension'))
    img.header.extensions.append(Nifti1Extension(4, bytes(range(1, 10))))
    img.header.set_data_dtype(np.float32)
    img.to_filename(path)
    # The same file, gzipped. The header and the extension area are unchanged by the compression, and our
    # reader must handle them the same way.
    with open(path, 'rb') as fh:
        payload = fh.read()
    with gzip.open(gzpath, 'wb') as fh:
        fh.write(payload)
    return 0


def main():
    if len(sys.argv) < 3:
        print(__doc__)
        return 1
    action = sys.argv[1]
    if action == 'dump':
        payload_dir = sys.argv[3] if len(sys.argv) > 3 else None
        return dump(sys.argv[2], payload_dir)
    elif action == 'write_reference':
        return write_reference(sys.argv[2], sys.argv[3])
    else:
        print(__doc__)
        return 1


if __name__ == '__main__':
    sys.exit(main())
