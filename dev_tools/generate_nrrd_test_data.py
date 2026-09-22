#!/usr/bin/env python3
"""Generate the NRRD test data for the freesurferformats R package.

The NRRD format has many variants (attached or detached header, four encodings, optional space
information, data files, skip fields), and the reader of the package has to handle all of them. The
files written by this script cover them, and for every file a dump of what the reference
implementations read from it is written next to it, so that the R reader can be compared against
them instead of against the expectations of the author of the R code:

  - `pynrrd` (which is what Python uses for NRRD, since nibabel has no NRRD support) provides the
    array shape, the data type and the full list of values.
  - `SimpleITK` (ITK, the origin of the NRRD format's space information) provides the image
    geometry for the files that store space directions, i.e. the origin, the spacing and the
    direction cosines, in ITK's LPS world coordinates. This is the independent implementation that
    the vox2ras computation of the R package is checked against.

Usage:
  python3 dev_tools/generate_nrrd_test_data.py generate <output_dir>
  python3 dev_tools/generate_nrrd_test_data.py read <file>.nrrd

The first form writes the test files and their dumps, the second one prints the dump for a single
existing file. `pynrrd` is required, `SimpleITK` is optional (its part of the dump is then skipped).
A subset of the generated files is copied to `inst/extdata/nrrd` so that the unit tests run
everywhere, including on CI, see README_DEVELOPMENT.md.

The `generate` command also writes a couple of files by hand, since pynrrd cannot produce them:
files that use the `line skip` and `byte skip` header fields, and one that uses the `data file: LIST`
mode, in which the header names several data files. They are checked with pynrrd as well, which
accepts them (this is what makes them valid test data).

Note that sheared (non-orthogonal) `space directions` cannot be expressed in ITK's image model, so
the geometry of the sheared fixture is not part of the ITK dump.

The generated data is tiny on purpose: NRRD is not a format for huge files here, the point is the
variety of the header variants, not the size.
"""

import os
import struct
import sys

import numpy as np
import nrrd


# ---------------------------------------------------------------------------------------------
# Data of the fixtures: small volumes with unique, easily recognizable values.
# ---------------------------------------------------------------------------------------------

def values_u8():
    """24 values, 1..24, in the order in which they are stored in the file."""
    return np.arange(1, 25, dtype=np.uint8)


def values_4d():
    """120 values, 1..120, for a 4D volume with 5 volumes of 4x3x2 voxels."""
    return np.arange(1, 121, dtype=np.uint8)


def values_dwi():
    """144 values, 1..144, for a DWI volume with 6 volumes of 4x3x2 voxels."""
    return np.arange(1, 145, dtype=np.uint16)


IDENTITY_DIRECTIONS = [(1.0, 0.0, 0.0), (0.0, 1.0, 0.0), (0.0, 0.0, 1.0)]

# A rotated (but orthonormal) basis, so that the geometry is non-trivial while remaining
# representable in ITK's image model (ITK stores a rotation and a spacing, not a shear).
ROTATED_DIRECTIONS = [(0.0, 1.0, 0.0), (-1.0, 0.0, 0.0), (0.0, 0.0, 1.0)]

# A sheared basis: ITK cannot express this, but the NRRD format can, and a reader has to handle it
# by building the matrix from the vectors as they are.
SHEARED_DIRECTIONS = [(1.5, 0.0, 0.0), (0.0, 2.0, 0.0), (0.5, 0.0, 3.0)]


# A row of NaN values in 'space directions' means 'none': the corresponding axis is not a space
# axis (e.g. the time axis of a 4D volume). This is the spelling that the NRRD format uses.
NO_DIRECTION = (float("nan"), float("nan"), float("nan"))


def write_fixtures(out_dir):
    """Write all test files. Returns the list of file names (relative to out_dir)."""
    written = []

    def save(name, data, header=None, compression_level=None, detached_header=False):
        path = os.path.join(out_dir, name)
        nrrd.write(path, data, header=header, index_order="F",
                   compression_level=compression_level, detached_header=detached_header)
        written.append(name)
        return path

    # --- 1. The simplest case: raw data, identity geometry, RAS space. ---
    save("vol_u8_raw.nrrd", values_u8().reshape((4, 3, 2), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": IDENTITY_DIRECTIONS,
        "space origin": (0.0, 0.0, 0.0),
        "encoding": "raw",
    })

    # --- 2. The same data in the ASCII encoding. ---
    save("vol_u8_ascii.nrrd", values_u8().reshape((4, 3, 2), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": IDENTITY_DIRECTIONS,
        "space origin": (0.0, 0.0, 0.0),
        "encoding": "ascii",
    })

    # --- 3. The same data gzip compressed. ---
    save("vol_u8_gzip.nrrd", values_u8().reshape((4, 3, 2), order="F"), compression_level=9, header={
        "space": "right-anterior-superior",
        "space directions": IDENTITY_DIRECTIONS,
        "space origin": (0.0, 0.0, 0.0),
        "encoding": "gzip",
    })

    # --- 4. The same data bzip2 compressed. ---
    save("vol_u8_bzip2.nrrd", values_u8().reshape((4, 3, 2), order="F"), compression_level=9, header={
        "space": "right-anterior-superior",
        "space directions": IDENTITY_DIRECTIONS,
        "space origin": (0.0, 0.0, 0.0),
        "encoding": "bzip2",
    })

    # --- 5. Another data type and byte order, with only 'spacings' and no space directions. ---
    save("vol_i16_be.nrrd", values_u8().astype(">i2").reshape((4, 3, 2), order="F"), header={
        "spacings": [2.0, 2.0, 3.0],
        "encoding": "raw",
    })

    # --- 6. float32 with a rotated (oblique) basis and an offset origin. ---
    save("vol_f32_oblique.nrrd", (values_u8().astype(np.float32) / 4.0).reshape((4, 3, 2), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": ROTATED_DIRECTIONS,
        "space origin": (10.0, -20.0, 30.0),
        "encoding": "raw",
    })

    # --- 7. float64 with a non-uniform, sheared basis. ITK cannot read the geometry of this one. ---
    save("vol_f64_sheared.nrrd", (values_u8().astype(np.float64) * 1.5).reshape((4, 3, 2), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": SHEARED_DIRECTIONS,
        "space origin": (-1.0, 2.0, -3.0),
        "encoding": "raw",
    })

    # --- 8. A volume in LPS space: vox2ras has to negate the first two axes. ---
    save("vol_u8_lps.nrrd", values_u8().reshape((4, 3, 2), order="F"), header={
        "space": "left-posterior-superior",
        "space directions": ROTATED_DIRECTIONS,
        "space origin": (5.0, 6.0, 7.0),
        "encoding": "raw",
    })

    # --- 9. A 4D volume: the 4th axis is not a space axis, so its space direction is 'none'. ---
    save("vol_u8_4d.nrrd", values_4d().reshape((4, 3, 2, 5), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": [IDENTITY_DIRECTIONS[0], IDENTITY_DIRECTIONS[1], IDENTITY_DIRECTIONS[2], NO_DIRECTION],
        "space origin": (0.0, 0.0, 0.0),
        "kinds": ["domain", "domain", "domain", "list"],
        "encoding": "raw",
    })

    # --- 10. No geometry information at all. ---
    save("vol_u8_no_geometry.nrrd", values_u8().reshape((4, 3, 2), order="F"), header={
        "encoding": "raw",
    })

    # --- 11. A detached header (.nhdr) with the data in a separate raw file. ---
    save("vol_u8_detached.nhdr", values_u8().reshape((4, 3, 2), order="F"), header={
        "space": "right-anterior-superior",
        "space directions": IDENTITY_DIRECTIONS,
        "space origin": (0.0, 0.0, 0.0),
        "encoding": "raw",
    }, detached_header=True)
    written.append("vol_u8_detached.raw")

    # --- 12. A DWI volume: the gradients are stored in custom header fields (the convention shared
    #         by teem, DTI-TK and 3D Slicer), and the measurement frame rotates them. ---
    dwi_header = {
        "space": "right-anterior-superior",
        "space directions": [IDENTITY_DIRECTIONS[0], IDENTITY_DIRECTIONS[1], IDENTITY_DIRECTIONS[2], NO_DIRECTION],
        "space origin": (0.0, 0.0, 0.0),
        "kinds": ["domain", "domain", "domain", "list"],
        "measurement frame": IDENTITY_DIRECTIONS,
        "DWMRI_b-value": "1000",
        "encoding": "raw",
    }
    gradients = [(0, 0, 0), (1, 0, 0), (0, 1, 0), (0, 0, 1),
                 (0.5773502691896258, 0.5773502691896258, 0.5773502691896258),
                 (-0.5773502691896258, 0.5773502691896258, 0.5773502691896258)]
    for volume_index, gradient in enumerate(gradients):
        dwi_header["DWMRI_gradient_%04d" % volume_index] = " ".join("%.16g" % value for value in gradient)
    save("vol_dwi.nrrd", values_dwi().reshape((4, 3, 2, 6), order="F"), header=dwi_header)

    # --- 13. All scalar data types. ---
    for type_name, dtype in [("i8", "i1"), ("u16", "u2"), ("i32", "i4"), ("u32", "u4"),
                             ("i64", "i8"), ("u64", "u8"), ("f32", "f4"), ("f64", "f8")]:
        save("vol_type_%s.nrrd" % type_name,
             values_u8().astype(dtype).reshape((4, 3, 2), order="F"),
             header={"space directions": IDENTITY_DIRECTIONS, "space origin": (0.0, 0.0, 0.0),
                     "encoding": "raw"})

    # --- 14. The header fields that only occur in files written by other software. pynrrd cannot
    #         write them, so they are written by hand here: the byte/line skip fields, and the
    #         'data file: LIST' mode, in which several data files hold the same volume. ---
    written.extend(write_handmade_fixtures(out_dir))

    return written


def hand_written_header(fields, lines=()):
    """Build a NRRD header from an ordered list of (key, value) pairs, plus extra lines."""
    text = "NRRD0004\n"
    for key, value in fields:
        text += "%s: %s\n" % (key, value)
    for line in lines:
        text += line + "\n"
    return text + "\n"


def write_handmade_fixtures(out_dir):
    """Write the fixtures that pynrrd cannot produce: skip fields and the data file LIST mode."""
    written = []
    data = values_u8()
    raw = data.tobytes()

    def write(name, text, payload):
        with open(os.path.join(out_dir, name), "wb") as fh:
            fh.write(text.encode("ascii"))
            fh.write(payload)
        written.append(name)

    base_fields = [("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"),
                   ("space", "right-anterior-superior"),
                   ("space directions", "(1,0,0) (0,1,0) (0,0,1)"), ("space origin", "(0,0,0)")]

    # Junk between the header and the data, announced by the byte skip field.
    junk = b"".join(struct.pack("B", index % 256) for index in range(32))
    write("vol_byteskip.nrrd",
          hand_written_header([("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"),
                               ("encoding", "raw"), ("byte skip", "32")]),
          junk + raw)

    # Junk text lines between the header and the data, announced by the line skip field.
    junk_lines = b"this is a line of junk\nand this is another one\n"
    write("vol_lineskip.nrrd",
          hand_written_header([("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"),
                               ("encoding", "raw"), ("line skip", "2")]),
          junk_lines + raw)

    # The byte skip value -1 means that the data is at the very end of the file, so it has to be
    # found by its size instead of by an offset from the header.
    write("vol_byteskip_minus1.nrrd",
          hand_written_header([("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"),
                               ("encoding", "raw"), ("byte skip", "-1")]),
          junk + raw)

    # The same for a gzip compressed file: the compressed stream starts right after the header and
    # ends at the end of the file, so it can only be read by streaming.
    import gzip
    compressed = gzip.compress(raw)
    write("vol_gzip_byteskip_minus1.nrrd",
          hand_written_header([("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"),
                               ("encoding", "gzip"), ("byte skip", "-1")]),
          compressed)

    # The LIST mode: the header names several data files, which together hold the volume. The two
    # halves of the data go into the two files, in the order in which they are listed.
    half = len(raw) // 2
    with open(os.path.join(out_dir, "vol_datafile_list_0.raw"), "wb") as fh:
        fh.write(raw[:half])
    with open(os.path.join(out_dir, "vol_datafile_list_1.raw"), "wb") as fh:
        fh.write(raw[half:])
    written.extend(["vol_datafile_list_0.raw", "vol_datafile_list_1.raw"])
    with open(os.path.join(out_dir, "vol_datafile_list.nhdr"), "wb") as fh:
        fh.write(hand_written_header(
            [("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"), ("encoding", "raw")],
            lines=["data file: LIST", "vol_datafile_list_0.raw", "vol_datafile_list_1.raw"]).encode("ascii"))
    written.append("vol_datafile_list.nhdr")

    # A whole-file gzip-compressed NRRD file ('.nrrd.gz'). The compressed file holds the uncompressed file
    # byte for byte, so it cannot be seeked in: the header has to be read through the decompression, and the
    # data follows right behind it.
    import gzip as gzip_module
    import shutil
    with open(os.path.join(out_dir, "vol_u8_raw.nrrd"), "rb") as fh:
        uncompressed = fh.read()
    with open(os.path.join(out_dir, "vol_u8_raw.nrrd.gz"), "wb") as fh:
        fh.write(gzip_module.compress(uncompressed))
    written.append("vol_u8_raw.nrrd.gz")

    # A detached header whose data file is gzip-compressed: the encoding field describes the content of the
    # data file, not of the header file. This is how the tools that write '.nhdr' files store compressed data.
    with open(os.path.join(out_dir, "vol_u8_detached_gz.raw.gz"), "wb") as fh:
        fh.write(gzip_module.compress(raw))
    written.append("vol_u8_detached_gz.raw.gz")
    with open(os.path.join(out_dir, "vol_u8_detached_gz.nhdr"), "wb") as fh:
        fh.write(hand_written_header(
            [("type", "uint8"), ("dimension", "3"), ("sizes", "4 3 2"), ("encoding", "gzip"),
             ("space", "right-anterior-superior"), ("space directions", "(1,0,0) (0,1,0) (0,0,1)"),
             ("space origin", "(0,0,0)"), ("data file", "vol_u8_detached_gz.raw.gz")]).encode("ascii"))
    written.append("vol_u8_detached_gz.nhdr")

    return written


# ---------------------------------------------------------------------------------------------
# Dumps: what the reference implementations read from a file.
# ---------------------------------------------------------------------------------------------

def parse_space_directions(value):
    """Parse a NRRD 'space directions' value into a list of rows, with None for 'none'."""
    rows = []
    for token in value.split():
        if token == "none":
            rows.append(None)
        else:
            rows.append(tuple(float(component) for component in token.strip("()").split(",")))
    return rows


def dump_file(path, sitk=None):
    """Return the dump for a single NRRD file as a list of text lines."""
    lines = ["nrrd_test_data_dump", "file %s" % os.path.basename(path)]
    data, header = nrrd.read(path, index_order="F")

    lines.append("dimension %d" % header["dimension"])
    lines.append("sizes %s" % " ".join(str(int(size)) for size in header["sizes"]))
    lines.append("type %s" % header["type"])
    lines.append("encoding %s" % header["encoding"])
    lines.append("endian %s" % header.get("endian", "none"))
    lines.append("space %s" % header.get("space", "none"))

    directions = header.get("space directions")
    if directions is None:
        lines.append("space_directions none")
    else:
        lines.append("space_directions %s" % " ".join("%.16g" % value for value in np.asarray(directions).ravel()))
    origin = header.get("space origin")
    lines.append("space_origin %s" % ("none" if origin is None
                                      else " ".join("%.16g" % value for value in np.asarray(origin).ravel())))
    spacings = header.get("spacings")
    lines.append("spacings %s" % ("none" if spacings is None
                                  else " ".join("%.16g" % value for value in np.asarray(spacings).ravel())))

    # The values, as a flat list in file order. This is what a reader has to reproduce.
    flat = np.asarray(data).ravel(order="F")
    lines.append("num_values %d" % flat.size)
    lines.append("values %s" % " ".join("%.16g" % value for value in flat))

    # The DWI metadata, if the file has any.
    for key in sorted(header.keys()):
        if key.startswith("DWMRI") or key.lower() == "measurement frame":
            value = header[key]
            if isinstance(value, np.ndarray):
                value = " ".join("%.16g" % component for component in value.ravel())
            lines.append("custom %s %s" % (key, value))

    # The geometry according to ITK, which is the reference implementation of the space information.
    if sitk is not None:
        try:
            image = sitk.ReadImage(path)
            origin = image.GetOrigin()
            spacing = image.GetSpacing()
            direction = image.GetDirection()
            lines.append("itk_origin %s" % " ".join("%.16g" % value for value in origin))
            lines.append("itk_spacing %s" % " ".join("%.16g" % value for value in spacing))
            lines.append("itk_direction %s" % " ".join("%.16g" % value for value in direction))
        except Exception as error:  # noqa: BLE001 - any ITK failure is reported in the dump
            lines.append("itk_error %s" % str(error).replace("\n", " "))

    return lines


# Files for which pynrrd cannot write a dump. pynrrd's header reader requires every header line to be a
# 'key: value' pair, so it rejects the NRRD 'data file: LIST' mode, in which the lines that follow the field
# name the data files (which is the syntax that the NRRD specification describes). It also cannot read a
# gzip-compressed NRRD file. The R check script verifies both of them structurally instead, by comparing
# their data against the uncompressed file that holds the same values (see dev_tools/check_nrrd_conversion.R).
NO_PYNRRD_DUMP = ["vol_datafile_list.nhdr", "vol_u8_raw.nrrd.gz"]


def dump_dir_name(out_dir):
    return os.path.join(out_dir, "expected")


def main(argv):
    if len(argv) < 2:
        print(__doc__)
        return 2

    try:
        import nrrd  # noqa: F401 - imported for the error message when it is missing
    except ImportError:
        print("ERROR: the 'pynrrd' package is required (pip install pynrrd).")
        return 2

    try:
        import SimpleITK as sitk
    except ImportError:
        sitk = None
        print("NOTE: SimpleITK is not available, the ITK part of the dumps is skipped.")

    command = argv[1]

    if command == "read":
        for line in dump_file(argv[2], sitk=sitk):
            print(line)
        return 0

    if command != "generate" or len(argv) < 3:
        print(__doc__)
        return 2

    out_dir = argv[2]
    os.makedirs(out_dir, exist_ok=True)
    os.makedirs(dump_dir_name(out_dir), exist_ok=True)

    written = write_fixtures(out_dir)
    print("Wrote %d files to '%s':" % (len(written), out_dir))
    for name in sorted(written):
        print("   %-32s %8d bytes" % (name, os.path.getsize(os.path.join(out_dir, name))))

    num_dumps = 0
    header_files = sorted(name for name in written if name.endswith((".nrrd", ".nhdr")))
    for name in header_files:
        if name in NO_PYNRRD_DUMP:
            print("NOTE: no pynrrd dump for '%s', see the NO_PYNRRD_DUMP comment." % name)
            continue
        lines = dump_file(os.path.join(out_dir, name), sitk=sitk)
        with open(os.path.join(dump_dir_name(out_dir), name + ".expected.txt"), "w") as fh:
            fh.write("\n".join(lines) + "\n")
        num_dumps += 1

    # Every other header file must have a dump, otherwise the R cross-validation script would silently
    # skip it.
    missing = [name for name in header_files
               if name not in NO_PYNRRD_DUMP
               and not os.path.exists(os.path.join(dump_dir_name(out_dir), name + ".expected.txt"))]
    if missing:
        print("ERROR: no dump was written for: %s" % ", ".join(missing))
        return 1

    print("Wrote %d expected dumps to '%s'." % (num_dumps, dump_dir_name(out_dir)))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
