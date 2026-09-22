#!/usr/bin/env python3
"""Dump the parsed CIFTI-2 XML metadata of CIFTI-2 files, using nibabel.

This is the nibabel reference dump for the 'fs.cifti' reader of freesurferformats:
it prints the same fields that the reader parses, in the same order, so that the
two implementations can be compared with a text diff. It uses nibabel's parsed
XML objects (not the axis conversion layer), i.e. a second, independent
implementation of the CIFTI-2 spec.

Note on mixed surface+volume files: nibabel 5.4.2 cannot *load* a CIFTI-2 file in
which the same brain structure has a surface brain model and a volume brain model
(Connectome Workbench writes such files, e.g. for '-cifti-create-dense-scalar
-left-metric ... -volume ...' when the structure label volume labels cortical
voxels): its axis layer raises 'Undefined vertex indices found for surface
elements'. For such files this tool falls back to extracting the XML from the
NIFTI-2 header extension and parsing it with nibabel's raw XML parser.

Usage: python3 dev_tools/cifti_dump.py FILE [FILE ...]
"""

import struct
import sys
from xml.sax.saxutils import escape

import numpy as np
import nibabel as nib
from nibabel.cifti2.parse_cifti2 import Cifti2Parser


def fmt_num(value):
    """Format a number in a way that R's sprintf("%.6f", x) reproduces exactly."""
    if value is None:
        return "NA"
    return "%.6f" % float(value)


def short_structure_name(name):
    """Normalize a brain structure name to the short form used by the reader."""
    if name is None:
        return "NA"
    return str(name).replace("CIFTI_STRUCTURE_", "")


def load_matrix(filepath):
    """Return (matrix, dim_sizes) of a CIFTI-2 file. Tries nibabel's image API first."""
    try:
        image = nib.load(filepath)
        return image.header.matrix, tuple(int(s) for s in image.shape)
    except Exception as exc:
        print("  note: nibabel could not load the file (%s), parsing the XML directly" % exc, file=sys.stderr)
    with open(filepath, "rb") as fh:
        raw = fh.read()
    # NIFTI-2 header: dim field at offset 16, eight 64-bit integers.
    dim = struct.unpack_from("<8q", raw, 16)
    dim_sizes = tuple(int(dim[5 + idx]) for idx in range(int(dim[0]) - 4))
    size, ecode = struct.unpack_from("<2i", raw, 544)
    if ecode != 32:
        raise SystemExit("File '%s' has no CIFTI XML extension (found code %d)." % (filepath, ecode))
    xml_bytes = raw[552:544 + size].rstrip(b"\x00")
    parser = Cifti2Parser()
    parser.parse(xml_bytes)
    return parser.header.matrix, dim_sizes


def dump_index_array(values):
    if values is None:
        return "NONE"
    if isinstance(values, np.ndarray):
        values = values.ravel().tolist()
    flat = []
    for value in values:
        if isinstance(value, (list, tuple, np.ndarray)):
            flat.extend(value)
        else:
            flat.append(value)
    return "%d vals: %s" % (len(flat), " ".join(str(int(v)) for v in flat))


def dump_metadata(metadata):
    lines = []
    for name, value in metadata.items():
        lines.append("  metadata[%s]=%s" % (name, escape(str(value)).replace("\n", " ").strip()))
    return lines


def dump_label_table(label_table):
    lines = []
    if label_table is None:
        return lines
    for key, label in sorted(label_table._labels.items()):
        lines.append("      label key=%d rgba=%s/%s/%s alpha=%s name=%s x=%s y=%s z=%s" % (
            int(key),
            fmt_num(label.red), fmt_num(label.green), fmt_num(label.blue),
            fmt_num(label.alpha), label.label,
            fmt_num(getattr(label, "x", None)), fmt_num(getattr(label, "y", None)), fmt_num(getattr(label, "z", None)),
        ))
    return lines


def dump_map(map_idx, im):
    lines = ["map %d dims=%s type=%s" % (
        map_idx,
        ",".join(str(d) for d in im.applies_to_matrix_dimension),
        im.indices_map_to_data_type,
    )]
    if im.number_of_series_points is not None:
        lines.append("  series points=%d start=%s step=%s exponent=%s unit=%s" % (
            int(im.number_of_series_points), fmt_num(im.series_start), fmt_num(im.series_step),
            "NA" if im.series_exponent is None else str(im.series_exponent), str(im.series_unit)))
    for surface in list(im.surfaces):
        lines.append("  surface structure=%s num_vertices=%d" % (
            surface.brain_structure, int(surface.surface_number_of_vertices)))
    if im.volume is not None:
        lines.append("  volume dims=%s meter_exponent=%s" % (
            ",".join(str(int(d)) for d in im.volume.volume_dimensions),
            str(im.volume.transformation_matrix_voxel_indices_ijk_to_xyz.meter_exponent)))
        matrix_xml = im.volume.transformation_matrix_voxel_indices_ijk_to_xyz.to_xml().decode("utf-8")
        matrix_text = matrix_xml[matrix_xml.index(">") + 1:matrix_xml.index("</")]
        lines.append("  volume matrix=%s" % " ".join(fmt_num(v) for v in matrix_text.split()))
    for bm_idx, brain_model in enumerate(list(im.brain_models)):
        lines.append("  brainmodel %d offset=%d count=%d type=%s structure=%s num_vertices=%s" % (
            bm_idx, int(brain_model.index_offset), int(brain_model.index_count),
            brain_model.model_type, brain_model.brain_structure,
            "NA" if brain_model.surface_number_of_vertices is None else int(brain_model.surface_number_of_vertices)))
        lines.append("    vertices %s" % dump_index_array(brain_model.vertex_indices))
        lines.append("    voxels %s" % dump_index_array(brain_model.voxel_indices_ijk))
    for parcel_idx, parcel in enumerate(list(im.parcels)):
        lines.append("  parcel %d name=%s" % (parcel_idx, parcel.name))
        for vertices in parcel.vertices:
            lines.append("    vertices structure=%s %s" % (
                short_structure_name(vertices.brain_structure), dump_index_array(list(vertices))))
        lines.append("    voxels %s" % dump_index_array(parcel.voxel_indices_ijk))
    for named_map_idx, named_map in enumerate(list(im.named_maps)):
        lines.append("  namedmap %d name=%s" % (named_map_idx, named_map.map_name or ""))
        lines.extend(dump_label_table(named_map.label_table))
    return lines


def dump_file(filepath):
    lines = ["file %s" % filepath]
    matrix, dim_sizes = load_matrix(filepath)
    lines.append("matrix dim_sizes=%s" % ",".join(str(s) for s in dim_sizes))
    lines.extend(dump_metadata(matrix.metadata))
    for map_idx, im in enumerate(matrix):
        lines.extend(dump_map(map_idx, im))
    return lines


def main():
    if len(sys.argv) < 2:
        raise SystemExit(__doc__)
    for filepath in sys.argv[1:]:
        print("\n".join(dump_file(filepath)))


if __name__ == "__main__":
    main()
