#!/usr/bin/env python3
"""Print a compact dump of a two-file image (ANALYZE 7.5 or a NIFTI v1 pair), read with nibabel.

Used by dev_tools/check_analyze_conversion.R to verify the files that freesurferformats writes with an
independent implementation: the R script writes a file, calls this script on it, and compares the output.

Usage:
    python3 dev_tools/nibabel_analyze_dump.py <file.hdr|file.img|file.hdr.gz|file.img.gz>

Prints lines of the form 'key value', one per line. Requires nibabel.
"""
import os
import sys
import numpy as np
import nibabel as nib
from nibabel.analyze import AnalyzeHeader
from nibabel.spm99analyze import Spm99AnalyzeHeader


def open_header(path):
    if path.endswith('.gz'):
        import gzip
        return gzip.open(path, 'rb')
    return open(path, 'rb')


def main():
    if len(sys.argv) != 2:
        print(__doc__)
        return 1
    path = sys.argv[1]
    img = nib.load(path)
    data = np.asarray(img.dataobj.get_unscaled())
    print("num_values %d" % data.size)
    print("shape %s" % ' '.join(str(v) for v in data.shape))
    print("sum %.10g" % float(data.sum()))
    print("min %.10g" % float(data.min()))
    print("max %.10g" % float(data.max()))
    print("dtype %s" % img.header.get_data_dtype())
    print("affine %s" % ' '.join(repr(float(v)) for v in img.affine.ravel()))

    # The header fields, read with the plain ANALYZE layout (the pair variant of NIFTI shares it, but uses other
    # names for some fields, so both are reported).
    header = AnalyzeHeader.from_fileobj(open_header(path))
    print("dim %s" % ' '.join(str(int(v)) for v in header['dim']))
    print("pixdim %s" % ' '.join(repr(float(v)) for v in header['pixdim']))
    print("magic %s" % bytes(header.binaryblock[344:348]).hex())
    spm_header = Spm99AnalyzeHeader.from_fileobj(open_header(path))
    print("spm_origin %s" % ' '.join(str(int(v)) for v in spm_header['origin'][:3]))
    print("scl_slope %.10g" % float(spm_header['scl_slope']))
    if os.path.exists(path):
        # nibabel only reads the SPM '.mat' sidecar if scipy is installed, and only for its SPM classes, so the
        # affine reported above is the one of the class that nibabel picked when loading the file.
        print("loaded_class %s" % type(img).__name__)
    return 0


if __name__ == '__main__':
    sys.exit(main())
