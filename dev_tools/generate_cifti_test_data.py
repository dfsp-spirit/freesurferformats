#!/usr/bin/env python3
"""Generate CIFTI-2 test data for freesurferformats.

Creates small CIFTI-2 files with a synthetic, tiny surface (10 left and 12 right
vertices) and a tiny volume (4x4x4), covering all nine standard CIFTI file types,
the surface and the volume brain model types, a reduced dense mapping (index
count smaller than the surface vertex count) and a mixed surface+volume file.

The CIFTI-2 files themselves are written by Connectome Workbench, i.e., by the
reference implementation, so they are exactly what other software produces and
expects. The input GIFTI/NIfTI files (metrics, labels, ROIs, volumes) are written
with nibabel, which is only a build-time dependency.

The commands run here are also the documentation of how the test data was made:
they go into a fixed build directory, and the provenance metadata that Workbench
stores in the CIFTI files is neutralized afterwards with

    Rscript dev_tools/neutralize_cifti_provenance.R

which removes the paths of the machine and of the checkout this ran in (so that
regenerating the fixtures elsewhere produces the same files, and so that the
package does not ship a path of a developer's machine). Run it before copying the
files into inst/extdata/cifti.

Usage:
    python3 dev_tools/generate_cifti_test_data.py --outdir /tmp/cifti_fixtures

Options:
    --outdir DIR      directory to write the generated CIFTI files to
    --builddir DIR    directory for the intermediate input files (default: a
                      subdirectory 'build' of --outdir)
    --wb_command PATH  path to wb_command (default: $WB_COMMAND, or the
                      developer's local install)

Requires: nibabel, numpy, and a Connectome Workbench installation.
"""

import argparse
import os
import shutil
import subprocess
import sys

import numpy as np
import nibabel as nib
from nibabel.gifti import GiftiImage, GiftiDataArray, GiftiMetaData, GiftiLabelTable, GiftiLabel

NUM_LH_VERTICES = 10
NUM_RH_VERTICES = 12
NUM_MAPS = 4          # number of maps/timepoints in the metric input files
VOLUME_DIM = 4        # the volume is VOLUME_DIM^3 voxels
NUM_VOLUME_TPS = 3    # number of timepoints of the volume timeseries

# CIFTI structure codes as used in the 'structure label volume' of Workbench.
# Determined empirically from the files Workbench writes (see probe_volume_labels()).
STRUCTURE_CORTEX_LEFT = 1
STRUCTURE_CORTEX_RIGHT = 2
STRUCTURE_CEREBELLUM = 3


def run(cmd):
    """Run a command, print it, and fail on error."""
    print("+ " + " ".join(cmd))
    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise RuntimeError("Command failed with exit code %d: %s" % (proc.returncode, " ".join(cmd)))
    return proc.stdout


def write_gifti_metric(filepath, data, names=None):
    """Write a GIFTI metric file (.shape.gii), one data array per map.

    `data` is a (num_vertices, num_maps) matrix.
    """
    img = GiftiImage()
    for map_idx in range(data.shape[1]):
        meta = GiftiMetaData()
        if names is not None:
            meta = GiftiMetaData({"Name": names[map_idx]})
        darray = GiftiDataArray(
            data=np.asarray(data[:, map_idx], dtype=np.float32),
            intent="NIFTI_INTENT_SHAPE",
            datatype="NIFTI_TYPE_FLOAT32",
            meta=meta,
        )
        img.add_gifti_data_array(darray)
    nib.save(img, filepath)


def write_gifti_label(filepath, keys, names):
    """Write a GIFTI label file (.label.gii) with a label table.

    `keys` is a vector of integer label keys per vertex, `names` maps key -> name.
    """
    img = GiftiImage()
    darray = GiftiDataArray(
        data=np.asarray(keys, dtype=np.int32),
        intent="NIFTI_INTENT_LABEL",
        datatype="NIFTI_TYPE_INT32",
    )
    img.add_gifti_data_array(darray)

    def label_table_entry(key, name, red, green, blue, alpha):
        # nibabel 5.4.2 does not accept a 'label' argument, but it reads the attribute
        # when writing the XML, so it has to be set afterwards.
        entry = GiftiLabel(key=key, red=red, green=green, blue=blue, alpha=alpha)
        entry.label = name
        return entry

    label_table = GiftiLabelTable()
    label_table.labels.append(label_table_entry(0, "???", 1.0, 1.0, 1.0, 0.0))
    for key, name in sorted(names.items()):
        label_table.labels.append(label_table_entry(key, name, 0.0, 0.5, 1.0, 1.0))
    img.labeltable = label_table
    nib.save(img, filepath)


def write_roi_metric(filepath, num_vertices, keep_even=True):
    """Write a metric file that is 1 for the vertices to keep and 0 for the others."""
    values = np.zeros((num_vertices, 1), dtype=np.float32)
    for idx in range(num_vertices):
        values[idx, 0] = 1.0 if (idx % 2 == 0) == keep_even else 0.0
    write_gifti_metric(filepath, values, names=["ROI"])


def write_volume(filepath, data, affine=None):
    """Write a NIfTI v1 volume with float32 data."""
    if affine is None:
        affine = np.diag([2.0, 2.0, 2.0, 1.0])  # 2 mm isotropic, no rotation
    img = nib.Nifti1Image(np.asarray(data, dtype=np.float32), affine)
    img.header.set_data_dtype(np.float32)
    nib.save(img, filepath)


def write_structure_volume(filepath, data, affine=None):
    """Write a NIfTI v1 volume with int16 data.

    The voxel values are CIFTI structure codes (0 means 'not part of the CIFTI
    file'). This is *not* yet a label volume in the sense of Workbench: the label
    table has to be added with '-volume-label-import', see make_label_volume().
    """
    if affine is None:
        affine = np.diag([2.0, 2.0, 2.0, 1.0])
    img = nib.Nifti1Image(np.asarray(data, dtype=np.int16), affine)
    img.header.set_data_dtype(np.int16)
    nib.save(img, filepath)


def make_label_volume(wb_command, raw_volume, filepath, names):
    """Turn an integer volume into a Workbench label volume.

    Workbench only accepts a real label volume (a volume with a label table in a
    NIfTI extension) as the 'structure label volume' argument of
    '-cifti-create-*', otherwise it fails with 'parcel volume is not of type label'.
    Setting the NIfTI intent code to NIFTI_INTENT_LABEL is not enough.
    """
    label_list = filepath + ".label_list.txt"
    with open(label_list, "w") as fh:
        for key, name in sorted(names.items()):
            fh.write("%s\n%d 255 0 0 1\n" % (name, key))
    run([wb_command, "-volume-label-import", raw_volume, label_list, filepath])
    return filepath


def make_inputs(builddir):
    """Write the metric, label, ROI and volume input files. Returns a dict of paths."""
    paths = {}
    paths["lh_metric"] = os.path.join(builddir, "lh.metric.shape.gii")
    paths["rh_metric"] = os.path.join(builddir, "rh.metric.shape.gii")
    paths["lh_roi"] = os.path.join(builddir, "lh.roi.shape.gii")
    paths["rh_roi"] = os.path.join(builddir, "rh.roi.shape.gii")
    paths["lh_label"] = os.path.join(builddir, "lh.label.label.gii")
    paths["rh_label"] = os.path.join(builddir, "rh.label.label.gii")
    paths["volume"] = os.path.join(builddir, "volume.nii")
    paths["volume_maps"] = os.path.join(builddir, "volume_maps.nii")
    paths["volume_ts"] = os.path.join(builddir, "volume_ts.nii")
    paths["volume_labels_raw"] = os.path.join(builddir, "volume_labels_raw.nii")

    # Metric files: deterministic, different value ranges for lh and rh so that a
    # mixup of the hemispheres or of the maps is detectable.
    lh_data = np.zeros((NUM_LH_VERTICES, NUM_MAPS), dtype=np.float64)
    rh_data = np.zeros((NUM_RH_VERTICES, NUM_MAPS), dtype=np.float64)
    for map_idx in range(NUM_MAPS):
        for vert_idx in range(NUM_LH_VERTICES):
            lh_data[vert_idx, map_idx] = 100.0 * (map_idx + 1) + vert_idx
        for vert_idx in range(NUM_RH_VERTICES):
            rh_data[vert_idx, map_idx] = 200.0 * (map_idx + 1) + vert_idx
    write_gifti_metric(paths["lh_metric"], lh_data, names=["lhmap%d" % (i + 1) for i in range(NUM_MAPS)])
    write_gifti_metric(paths["rh_metric"], rh_data, names=["rhmap%d" % (i + 1) for i in range(NUM_MAPS)])

    write_roi_metric(paths["lh_roi"], NUM_LH_VERTICES, keep_even=True)
    write_roi_metric(paths["rh_roi"], NUM_RH_VERTICES, keep_even=True)

    parcel_names = {1: "PARCEL_A", 2: "PARCEL_B", 3: "PARCEL_C"}
    lh_keys = np.array([0, 1, 1, 2, 2, 3, 3, 1, 2, 3], dtype=np.int32)
    rh_keys = np.array([3, 3, 2, 2, 1, 1, 0, 0, 1, 2, 3, 3], dtype=np.int32)
    write_gifti_label(paths["lh_label"], lh_keys, parcel_names)
    write_gifti_label(paths["rh_label"], rh_keys, parcel_names)

    # A small volume: CEREBELLUM in the lower half, CORTEX_LEFT and CORTEX_RIGHT in the upper half.
    # Voxels with label 0 are excluded from the CIFTI file.
    label_vol = np.zeros((VOLUME_DIM, VOLUME_DIM, VOLUME_DIM), dtype=np.int16)
    label_vol[:, :, VOLUME_DIM // 2:] = STRUCTURE_CORTEX_LEFT
    label_vol[:, :, : VOLUME_DIM // 2] = STRUCTURE_CEREBELLUM
    label_vol[0, 0, 0] = 0  # exclude one voxel, so that the voxel indices are not simply all voxels
    label_vol[0, 0, 1] = STRUCTURE_CORTEX_RIGHT
    write_structure_volume(paths["volume_labels_raw"], label_vol)

    vol_data = np.zeros(label_vol.shape, dtype=np.float64)
    for x in range(VOLUME_DIM):
        for y in range(VOLUME_DIM):
            for z in range(VOLUME_DIM):
                vol_data[x, y, z] = 1000.0 + 100.0 * x + 10.0 * y + z
    write_volume(paths["volume"], vol_data)

    # A volume with as many maps as the metric files, to be combined with them in one file.
    vol_maps = np.zeros(label_vol.shape + (NUM_MAPS,), dtype=np.float64)
    for map_idx in range(NUM_MAPS):
        vol_maps[:, :, :, map_idx] = vol_data + 100.0 * (map_idx + 1)
    write_volume(paths["volume_maps"], vol_maps)

    vol_ts = np.zeros(label_vol.shape + (NUM_VOLUME_TPS,), dtype=np.float64)
    for tp in range(NUM_VOLUME_TPS):
        vol_ts[:, :, :, tp] = vol_data + 10000.0 * (tp + 1)
    write_volume(paths["volume_ts"], vol_ts)

    return paths


def check_structure_labels(wb_command, cifti_file):
    """Check which CIFTI structures Workbench created from our structure label volume.

    Returns the set of brain structure names in the file. This verifies that the
    structure codes used in make_inputs() are the ones Workbench expects (they were
    determined empirically this way).
    """
    import re
    xml_text = run([wb_command, "-nifti-information", cifti_file, "-print-xml"])
    return set(match.decode("ascii") for match in re.findall(rb"CIFTI_STRUCTURE_[A-Z_]+", xml_text.encode("utf-8")))


def make_cifti_files(wb_command, builddir):
    """Run the Workbench commands that build the CIFTI-2 files."""
    paths = make_inputs(builddir)
    paths["volume_labels"] = make_label_volume(
        wb_command,
        paths["volume_labels_raw"],
        os.path.join(builddir, "volume_labels.nii"),
        {STRUCTURE_CORTEX_LEFT: "CORTEX_LEFT",
         STRUCTURE_CORTEX_RIGHT: "CORTEX_RIGHT",
         STRUCTURE_CEREBELLUM: "CEREBELLUM"},
    )
    out = {}

    # Dense scalar with two surface structures (lh: 10, rh: 12 vertices).
    out["dscalar"] = os.path.join(builddir, "tiny.dscalar.nii")
    run([wb_command, "-cifti-create-dense-scalar", out["dscalar"],
         "-left-metric", paths["lh_metric"], "-right-metric", paths["rh_metric"]])

    # Dense scalar with a reduced mapping: only the even vertices are kept, so the
    # index count is smaller than the surface vertex count and the vertex indices
    # are not contiguous.
    out["dscalar_roi"] = os.path.join(builddir, "tiny_roi.dscalar.nii")
    run([wb_command, "-cifti-create-dense-scalar", out["dscalar_roi"],
         "-left-metric", paths["lh_metric"], "-roi-left", paths["lh_roi"],
         "-right-metric", paths["rh_metric"], "-roi-right", paths["rh_roi"]])

    # Dense timeseries (4 timepoints), with a 2.5 second timestep.
    out["dtseries"] = os.path.join(builddir, "tiny.dtseries.nii")
    run([wb_command, "-cifti-create-dense-timeseries", out["dtseries"],
         "-left-metric", paths["lh_metric"], "-right-metric", paths["rh_metric"],
         "-timestep", "2.5"])

    # Dense label file (label maps with a label table).
    out["dlabel"] = os.path.join(builddir, "tiny.dlabel.nii")
    run([wb_command, "-cifti-create-label", out["dlabel"],
         "-left-label", paths["lh_label"], "-right-label", paths["rh_label"]])

    # Dense scalar and timeseries with volume structures only.
    out["dscalar_volume"] = os.path.join(builddir, "tiny_volume.dscalar.nii")
    run([wb_command, "-cifti-create-dense-scalar", out["dscalar_volume"],
         "-volume", paths["volume"], paths["volume_labels"]])

    out["dtseries_volume"] = os.path.join(builddir, "tiny_volume.dtseries.nii")
    run([wb_command, "-cifti-create-dense-timeseries", out["dtseries_volume"],
         "-volume", paths["volume_ts"], paths["volume_labels"], "-timestep", "1.5"])

    # Mixed surface and volume structures in one file. All inputs must have the same
    # number of maps, so the 4-map volume is used here.
    out["dscalar_mixed"] = os.path.join(builddir, "tiny_mixed.dscalar.nii")
    run([wb_command, "-cifti-create-dense-scalar", out["dscalar_mixed"],
         "-left-metric", paths["lh_metric"], "-roi-left", paths["lh_roi"],
         "-right-metric", paths["rh_metric"],
         "-volume", paths["volume_maps"], paths["volume_labels"]])

    # Dense connectome: correlation of the rows of the dense timeseries. This file
    # has a single MatrixIndicesMap that applies to both dimensions.
    out["dconn"] = os.path.join(builddir, "tiny.dconn.nii")
    run([wb_command, "-cifti-correlation", out["dtseries"], out["dconn"]])

    # Parcellated files, using the label file as the parcellation. Note that
    # '-cifti-parcellate' only knows the directions ROW and COLUMN (there is no
    # BOTH), so a parcellated connectome is made in two steps.
    out["pscalar"] = os.path.join(builddir, "tiny.pscalar.nii")
    run([wb_command, "-cifti-parcellate", out["dscalar"], out["dlabel"], "COLUMN", out["pscalar"]])

    out["ptseries"] = os.path.join(builddir, "tiny.ptseries.nii")
    run([wb_command, "-cifti-parcellate", out["dtseries"], out["dlabel"], "COLUMN", out["ptseries"]])

    # Dense/parcel connectomes: parcellate one of the two dimensions of the dconn.
    out["dpconn"] = os.path.join(builddir, "tiny.dpconn.nii")
    run([wb_command, "-cifti-parcellate", out["dconn"], out["dlabel"], "COLUMN", out["dpconn"]])

    out["pdconn"] = os.path.join(builddir, "tiny.pdconn.nii")
    run([wb_command, "-cifti-parcellate", out["dconn"], out["dlabel"], "ROW", out["pdconn"]])

    # Parcel/parcel connectome: parcellate the remaining dense dimension of the dpconn.
    out["pconn"] = os.path.join(builddir, "tiny.pconn.nii")
    run([wb_command, "-cifti-parcellate", out["dpconn"], out["dlabel"], "ROW", out["pconn"]])

    return paths, out


def describe(wb_command, filepath):
    """Print what Workbench says about a CIFTI file."""
    text = run([wb_command, "-nifti-information", filepath, "-print-header"])
    lines = [line.strip() for line in text.splitlines()]
    keep = [line for line in lines if line.startswith(("dim[", "datatype", "intent", "vox_offset", "scl_"))]
    return ", ".join(keep)


def main():
    parser = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    parser.add_argument("--outdir", required=True, help="directory to write the generated CIFTI files to")
    parser.add_argument("--builddir", default=None, help="directory for intermediate files")
    parser.add_argument("--wb_command", default=None, help="path to the wb_command executable")
    args = parser.parse_args()

    wb_command = args.wb_command or os.environ.get("WB_COMMAND")
    if wb_command is None:
        wb_command = os.path.expanduser("~/software/connectome_workbench/workbench/bin_linux64/wb_command")
    if not os.path.isfile(wb_command):
        raise SystemExit("wb_command not found at '%s', use --wb_command." % wb_command)

    builddir = args.builddir or os.path.join(args.outdir, "build")
    if not os.path.isdir(builddir):
        os.makedirs(builddir)
    if not os.path.isdir(args.outdir):
        os.makedirs(args.outdir)

    print("Writing input files ...")
    paths, cifti_files = make_cifti_files(wb_command, builddir)

    print("Checking the structure labels used in the label volume ...")
    found = check_structure_labels(wb_command, cifti_files["dscalar_volume"])
    print("  Workbench created structures: %s" % ", ".join(sorted(found)))
    expected = {"CIFTI_STRUCTURE_CORTEX_LEFT", "CIFTI_STRUCTURE_CORTEX_RIGHT", "CIFTI_STRUCTURE_CEREBELLUM"}
    if found != expected:
        raise SystemExit("Unexpected structure labels: %s, expected %s" % (found, expected))

    print("Copying CIFTI files to '%s' ..." % args.outdir)
    for key, src in cifti_files.items():
        dest = os.path.join(args.outdir, os.path.basename(src))
        shutil.copyfile(src, dest)
        print("  %-32s %8d bytes  %s" % (os.path.basename(dest), os.path.getsize(dest), describe(wb_command, dest)))

    print("Done.")


if __name__ == "__main__":
    main()
