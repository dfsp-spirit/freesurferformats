#!/usr/bin/env python3
"""Generate the VTK legacy format test data for the freesurferformats R package.

The files are written by VTK itself (through pyvista), so that the reader tests
compare the R implementation against an independent implementation instead of
against the package's own writer. Every generated file is accompanied by a
'*.expected.txt' dump of what VTK reads back from it, and that dump is what the
R tests compare their results against.

The expected dumps are also used in the other direction: the R development
script 'dev_tools/check_vtk_conversion.R' writes files with the R writer and asks
this script to dump them with VTK, which verifies the R writer against VTK.

Usage:
    /path/to/python dev_tools/generate_vtk_test_data.py

Requirements: numpy, pyvista (which brings the VTK bindings).

The files currently committed to this repository were generated with
pyvista 0.48.4 / VTK 9.6.2 under Python 3.12.
"""

import sys
import os
import gzip
import argparse

try:
    import numpy as np
    import pyvista as pv
    import vtk
    from vtk.util.numpy_support import vtk_to_numpy
except ImportError as exc:  # pragma: no cover
    sys.stderr.write("This script requires numpy and pyvista: %s\n" % exc)
    sys.exit(1)


DUMP_VERSION = "vtk_test_data_dump 1"


def cell_records(cell_array):
    """Return the cells of a vtkCellArray as a list of index lists.

    Uses the offsets and connectivity arrays, which are what VTK stores
    internally in all supported versions, instead of the deprecated flat
    representation.
    """
    offsets = vtk_to_numpy(cell_array.GetOffsetsArray())
    connectivity = vtk_to_numpy(cell_array.GetConnectivityArray())
    return [connectivity[offsets[i]:offsets[i + 1]] for i in range(len(offsets) - 1)]


def dump_polydata(polydata):
    """Format the geometry of a vtkPolyData object as text.

    The format is line based and easy to parse from R: a keyword and a count
    followed by the values, with a count of -1 meaning that the file has no such
    cell array at all (as opposed to having an empty one).
    """
    lines = [DUMP_VERSION]
    points = vtk_to_numpy(polydata.GetPoints().GetData())
    lines.append("points %d" % len(points))
    for point in points:
        lines.append(" ".join(repr(float(coord)) for coord in point))
    for name, keyword in (("polys", "GetPolys"), ("lines", "GetLines"), ("verts", "GetVerts")):
        cell_array = getattr(polydata, keyword)()
        if cell_array is None or cell_array.GetNumberOfCells() == 0:
            lines.append("%s -1" % name)
            continue
        records = cell_records(cell_array)
        lines.append("%s %d" % (name, len(records)))
        for record in records:
            lines.append(" ".join(str(int(idx)) for idx in record))
    return "\n".join(lines) + "\n"


def read_legacy(path):
    """Read a VTK legacy file with VTK itself."""
    reader = vtk.vtkPolyDataReader()
    reader.SetFileName(path)
    reader.ReadAllScalarsOn()
    reader.ReadAllVectorsOn()
    reader.ReadAllNormalsOn()
    reader.ReadAllTCoordsOn()
    reader.Update()
    polydata = reader.GetOutput()
    if polydata is None or polydata.GetNumberOfPoints() == 0:
        raise RuntimeError("VTK could not read back '%s'." % path)
    return polydata


def write_legacy(mesh, path, file_version, binary):
    """Write a vtkPolyData object as a VTK legacy file."""
    writer = vtk.vtkPolyDataWriter()
    writer.SetFileName(path)
    writer.SetInputData(mesh)
    if binary:
        writer.SetFileTypeToBinary()
    else:
        writer.SetFileTypeToASCII()
    writer.SetFileVersion(file_version)
    if writer.Write() != 1:
        raise RuntimeError("Failed to write '%s'." % path)


def int_width(type_name):
    """The number of bytes of a VTK integer type name like 'vtktypeint64'."""
    return int(type_name.split("vtktypeint")[1]) // 8


def next_section_offset(blob, start):
    """Offset of the next section keyword after start, or the end of the data.

    A single trailing byte is allowed at the end of the file: VTK appends a
    newline after the last data section of a binary file.
    """
    positions = [blob.find(b"\n" + keyword, start) for keyword in (
        b"POINTS ", b"VERTICES ", b"LINES ", b"POLYGONS ", b"OFFSETS ",
        b"CONNECTIVITY ", b"POINT_DATA ", b"CELL_DATA ")]
    positions = [position for position in positions if position >= 0]
    return min(positions) if positions else len(blob)


def check_block_size(path, actual, expected, what):
    """Check the size of a binary data block, allowing a trailing newline."""
    if actual != expected and actual != expected + 1:
        raise RuntimeError("%s: the %s has %d bytes, expected %d." % (path, what, actual, expected))
    return "%d bytes%s" % (actual, " incl. the trailing newline" if actual != expected else "")


def describe_binary_layout(path):
    """Report the cell array layout and element sizes of a binary VTK file.

    The legacy format does not record either the cell array layout or the width
    of the integers, so the reader has to detect both. This function documents
    what the generated files actually contain, and verifies the element widths
    against the sizes of the data blocks, which is the empirical evidence the R
    reader is based on.
    """
    import struct

    with open(path, "rb") as handle:
        blob = handle.read()

    positions = [blob.find(b"\n" + keyword) for keyword in (b"POLYGONS ", b"LINES ", b"VERTICES ")]
    positions = [position for position in positions if position >= 0]
    if not positions:
        return "no cell array"
    pos = min(positions) + 1
    line_end = blob.index(b"\n", pos)
    header = blob[pos:line_end].decode("ascii")
    counts = [int(token) for token in header.split()[1:]]
    data_start = line_end + 1

    if blob[data_start:data_start + 8] == b"OFFSETS ":
        offsets_line_end = blob.index(b"\n", data_start)
        offsets_type = blob[data_start:offsets_line_end].decode("ascii").split()[1]
        offsets_width = int_width(offsets_type)
        offsets_data_start = offsets_line_end + 1
        connectivity_pos = blob.index(b"\nCONNECTIVITY ", offsets_data_start)
        connectivity_line_end = blob.index(b"\n", connectivity_pos + 1)
        connectivity_type = blob[connectivity_pos + 1:connectivity_line_end].decode("ascii").split()[1]
        connectivity_width = int_width(connectivity_type)
        connectivity_data_start = connectivity_line_end + 1
        data_end = next_section_offset(blob, connectivity_data_start)

        offsets_info = check_block_size(path, connectivity_pos - offsets_data_start,
                                       counts[0] * offsets_width, "offsets array")
        connectivity_info = check_block_size(path, data_end - connectivity_data_start,
                                             counts[1] * connectivity_width, "connectivity array")
        layout = "new layout, OFFSETS (%s, %s) + CONNECTIVITY (%s, %s)" % (
            offsets_type, offsets_info, connectivity_type, connectivity_info)
    else:
        first32 = struct.unpack(">i", blob[data_start:data_start + 4])[0]
        if 0 < first32 <= 4:  # the vertex count of the first cell is stored in the first value
            width = 4
            first_value = first32
        else:
            width = 8
            first_value = struct.unpack(">q", blob[data_start:data_start + 8])[0]
        data_end = next_section_offset(blob, data_start)
        size_info = check_block_size(path, data_end - data_start, counts[1] * width, "cell array")
        layout = "old layout, counts + indices (%d byte values, %s, first cell has %d vertices)" % (
            width, size_info, first_value)

    return "%s | %s | %d bytes total" % (header, layout, file_size(path))


def file_size(path):
    return os.path.getsize(path)


def section_keywords(path):
    """Return the section keywords of a VTK file, in the order they occur.

    Only used to document what the generated attribute file contains for the
    people reading the tests later on.
    """
    known = [
        b"POINTS", b"VERTICES", b"LINES", b"POLYGONS", b"TRIANGLE_STRIPS",
        b"OFFSETS", b"CONNECTIVITY", b"POINT_DATA", b"CELL_DATA", b"SCALARS",
        b"VECTORS", b"NORMALS", b"TEXTURE_COORDINATES", b"LOOKUP_TABLE", b"FIELD",
    ]
    with open(path, "rb") as handle:
        blob = handle.read()
    found = []
    position = 0
    while position < len(blob):
        hits = [(blob.find(b"\n" + keyword + b" ", position), keyword) for keyword in known]
        hits = [hit for hit in hits if hit[0] >= 0]
        if not hits:
            break
        position, keyword = min(hits)
        found.append(keyword.decode("ascii"))
        position += 1
    return found


def write_and_dump(mesh, outdir, name, file_version, binary, dump=True):
    """Write a mesh, read it back with VTK, verify it and dump the geometry."""
    path = os.path.join(outdir, name + ".vtk")
    write_legacy(mesh, path, file_version, binary)

    read_back = read_legacy(path)
    if read_back.GetNumberOfPoints() != mesh.GetNumberOfPoints():
        raise RuntimeError("%s: VTK read back %d instead of %d points." % (
            name, read_back.GetNumberOfPoints(), mesh.GetNumberOfPoints()))
    if read_back.GetNumberOfCells() != mesh.GetNumberOfCells():
        raise RuntimeError("%s: VTK read back %d instead of %d cells." % (
            name, read_back.GetNumberOfCells(), mesh.GetNumberOfCells()))

    info = "%s: %d points, %d cells" % (name, read_back.GetNumberOfPoints(), read_back.GetNumberOfCells())
    if binary:
        info += " | " + describe_binary_layout(path)
    print(info)

    if dump:
        with open(os.path.join(outdir, "expected", name + ".expected.txt"), "w") as handle:
            handle.write(dump_polydata(read_back))
    return path


def make_sphere():
    """A small deterministic sphere mesh."""
    return pv.Sphere(radius=1.0, theta_resolution=8, phi_resolution=8)


def make_lines():
    """A small collection of polylines with different lengths."""
    points = np.array([
        [0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [2.0, 0.0, 0.0], [3.0, 0.0, 0.0],
        [0.0, 1.0, 0.0], [0.0, 2.0, 0.0], [0.0, 3.0, 0.0],
        [1.0, 1.0, 1.0], [1.5, 1.5, 1.5], [2.0, 2.0, 2.0], [2.5, 2.5, 2.5],
        [5.0, 5.0, 5.0],  # not part of any streamline
    ])
    line_indices = [[0, 1, 2, 3], [4, 5, 6], [7, 8, 9, 10]]
    flat = np.concatenate([np.concatenate([[len(line)], line]) for line in line_indices])
    return pv.PolyData(points, lines=flat)


def generate(outdir):
    """Write all test files into outdir."""
    os.makedirs(os.path.join(outdir, "expected"), exist_ok=True)

    sphere = make_sphere()
    for file_version in (42, 51):
        for binary in (False, True):
            name = "sphere_v%d_%s" % (file_version, "binary" if binary else "ascii")
            write_and_dump(sphere, outdir, name, file_version, binary)

    sphere_double = sphere.copy()
    sphere_double.points = sphere_double.points.astype(np.float64) * (1.0 + 1.0 / 3.0)
    write_and_dump(sphere_double, outdir, "sphere_v51_binary_float64", 51, True)

    attributed = make_sphere()
    attributed.point_data["my_scalars"] = np.arange(attributed.n_points, dtype=np.float32)
    attributed.point_data["my_vectors"] = np.tile(np.array([[0.0, 0.0, 1.0]], dtype=np.float32), (attributed.n_points, 1))
    attributed = attributed.compute_normals(cell_normals=False, point_normals=True, inplace=False)
    write_and_dump(attributed, outdir, "sphere_v51_binary_attributes", 51, True)
    print("  sections of sphere_v51_binary_attributes: %s" % ", ".join(
        section_keywords(os.path.join(outdir, "sphere_v51_binary_attributes.vtk"))))

    lines = make_lines()
    write_and_dump(lines, outdir, "lines_v42_ascii", 42, False)
    write_and_dump(lines, outdir, "lines_v51_binary", 51, True)

    cloud_points = np.array([[0.0, 0.0, 0.0], [1.0, 0.0, 0.0], [0.0, 2.0, 0.0]])
    cloud = pv.PolyData(cloud_points, verts=np.array([1, 0, 1, 1, 1, 2]))
    write_and_dump(cloud, outdir, "cloud_v51_binary", 51, True)

    plane = pv.Plane(i_resolution=1, j_resolution=1)  # a single quad, i.e. not a triangular mesh
    write_and_dump(plane, outdir, "quad_v51_ascii", 51, False)

    grid = pv.ImageData(dimensions=(3, 3, 3))
    grid_path = os.path.join(outdir, "imagedata_v51_binary.vtk")
    writer = vtk.vtkDataSetWriter()
    writer.SetFileName(grid_path)
    writer.SetInputData(grid)
    writer.SetFileTypeToBinary()
    writer.Write()
    print("imagedata_v51_binary: written as DATASET STRUCTURED_POINTS (not a POLYDATA)")

    return 0


def dump(path, outpath):
    """Write the VTK dump of an existing file, for verifying the R writer."""
    text = dump_polydata(read_legacy(path))
    if outpath:
        with open(outpath, "w") as handle:
            handle.write(text)
    else:
        sys.stdout.write(text)
    return 0


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    subparsers = parser.add_subparsers(dest="command")
    parser_gen = subparsers.add_parser("generate", help="generate the test data")
    parser_gen.add_argument("outdir", help="output directory")
    parser_read = subparsers.add_parser("read", help="dump a file with VTK")
    parser_read.add_argument("path", help="the VTK file to read")
    parser_read.add_argument("--out", help="write the dump here instead of to stdout")
    args = parser.parse_args()

    if args.command == "generate":
        return generate(args.outdir)
    if args.command == "read":
        return dump(args.path, args.out)
    parser.print_help()
    return 1


if __name__ == "__main__":
    sys.exit(main())
