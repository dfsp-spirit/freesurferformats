#!/usr/bin/env python3
"""Generate the small connectome test files for freesurferformats.

The CIFTI-2 connectome file types (`.dconn`, `.pconn`) are the ones whose payload cannot be
shipped as a fixture: a real dense connectome of an HCP subject has 91,282 x 91,282 values
(33 GB), and even the cortical-only version has 16.9 GB. The files this script creates are
small enough to keep in `extra_test_data/cifti` (a few hundred KB) but have *real* geometry:
they are computed by Connectome Workbench from the official Conte69 example files that are
already in the repository, so their mappings are the reduced, non-contiguous grayordinate
mappings of those files (30,424 left and 30,527 right indices for 32,492 vertex surfaces)
instead of the tiny synthetic ones of the shipped fixtures.

Created files (all written by Workbench, i.e. by the reference implementation):

  * `conte69.restricted300.dtseries.nii` - the official `.dtseries` with its dense mapping
    restricted to about 300 grayordinates (`-cifti-restrict-dense-map`). This is the file
    that shows how a reduced mapping looks: the index count is much smaller than the number
    of surface vertices, and the vertex indices are not contiguous.
  * `conte69.restricted300.dconn.nii` - the correlation of that file
    (`-cifti-correlation`), a 300 x 300 `.dconn` with a real dense mapping.
  * `conte69.ptseries.corr.pconn.nii` - the correlation of the official `.ptseries`, a
    54 x 54 `.pconn` whose parcels are the real Conte69 parcels (with their vertex lists
    for both hemispheres).

Usage:
    python3 dev_tools/generate_cifti_connectome_test_data.py

Requires: nibabel, numpy, a Connectome Workbench installation (`wb_command`), and the
official example files in `extra_test_data/cifti` (see extra_test_data/README).

The provenance metadata that Workbench writes into the files contains the paths of the
machine this script runs on. Neutralize them before committing, by running

    Rscript dev_tools/neutralize_cifti_provenance.R --dir extra_test_data/cifti

(which also verifies that rewriting the header extension with this package keeps the data,
the axes and the rest of the metadata intact).
"""

import argparse
import os
import subprocess
import sys

import numpy as np
import nibabel as nib
from nibabel.gifti import GiftiImage, GiftiDataArray

REPO_DIR = "/home/ts/develop/freesurferformats"
DEFAULT_DATA_DIR = os.path.join(REPO_DIR, "extra_test_data", "cifti")
DEFAULT_WB = os.path.expanduser("~/software/connectome_workbench/workbench/bin_linux64/wb_command")

# Keep every n-th grayordinate of the official mapping, which gives about 150 indices per
# hemisphere and thus a 300 x 300 dense connectome (360 KB).
KEEP_EVERY = 200


def run(cmd):
    print("+ " + " ".join(cmd))
    proc = subprocess.run(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
    if proc.returncode != 0:
        sys.stderr.write(proc.stdout)
        sys.stderr.write(proc.stderr)
        raise RuntimeError("Command failed with exit code %d: %s" % (proc.returncode, " ".join(cmd)))
    return proc.stdout


def write_roi_metric(filepath, num_vertices, keep_vertices):
    """Write a metric file (.shape.gii) that is 1 for the vertices to keep, 0 elsewhere."""
    values = np.zeros((num_vertices, 1), dtype=np.float32)
    values[list(keep_vertices), 0] = 1.0
    img = GiftiImage()
    img.add_gifti_data_array(GiftiDataArray(data=values[:, 0], intent="NIFTI_INTENT_SHAPE",
                                            datatype="NIFTI_TYPE_FLOAT32"))
    nib.save(img, filepath)


def grayordinate_indices(cifti_file):
    """Return the (structure, surface vertex index) pairs of matrix dimension 1 of a file."""
    img = nib.load(cifti_file)
    brain_models = img.header.get_index_map(1).brain_models
    result = {}
    for model in brain_models:
        structure = str(model.brain_structure)
        result[structure] = np.asarray(model.vertex_indices, dtype=int)
        print("  %s: %d indices of %d surface vertices" % (structure, len(result[structure]),
                                                           model.surface_number_of_vertices))
    return result


def main():
    parser = argparse.ArgumentParser(description="Generate the small connectome test data.")
    parser.add_argument("--datadir", default=DEFAULT_DATA_DIR)
    parser.add_argument("--builddir", default="/tmp/cifti_connectome_build")
    parser.add_argument("--wb_command", default=os.environ.get("WB_COMMAND", DEFAULT_WB))
    args = parser.parse_args()

    dtseries = os.path.join(args.datadir, "Conte69.MyelinAndCorrThickness.32k_fs_LR.dtseries.nii")
    ptseries = os.path.join(args.datadir, "Conte69.MyelinAndCorrThickness.32k_fs_LR.ptseries.nii")
    for path in (dtseries, ptseries):
        if not os.path.exists(path):
            raise SystemExit("The official example file '%s' is missing." % path)
    os.makedirs(args.builddir, exist_ok=True)

    # 1. The ROI metrics: the vertices of the official mapping, but only every KEEP_EVERY-th
    #    of them. The ROI has to cover the complete surface, the indices that are not in the
    #    mapping of the file are simply not used by the restriction.
    print("Building the ROI metrics:")
    indices = grayordinate_indices(dtseries)
    keep = {"lh": [], "rh": []}
    for structure, vertex_indices in indices.items():
        short = "lh" if "LEFT" in structure else "rh"
        keep[short] = vertex_indices[::KEEP_EVERY]
        print("  keeping %d of the %d %s indices" % (len(keep[short]), len(vertex_indices), short))

    # The surface size of the brain models is what the ROI metric has to have, since it is
    # indexed by surface vertex.
    surface_vertices = {}
    img = nib.load(dtseries)
    for model in img.header.get_index_map(1).brain_models:
        short = "lh" if "LEFT" in str(model.brain_structure) else "rh"
        surface_vertices[short] = int(model.surface_number_of_vertices)
    lh_roi = os.path.join(args.builddir, "lh.roi.shape.gii")
    rh_roi = os.path.join(args.builddir, "rh.roi.shape.gii")
    write_roi_metric(lh_roi, surface_vertices["lh"], keep["lh"])
    write_roi_metric(rh_roi, surface_vertices["rh"], keep["rh"])

    # 2. Restrict the dense mapping to those grayordinates.
    restricted = os.path.join(args.datadir, "conte69.restricted300.dtseries.nii")
    run([args.wb_command, "-cifti-restrict-dense-map", dtseries, "COLUMN", restricted,
         "-left-roi", lh_roi, "-right-roi", rh_roi])

    # 3. The connectomes.
    dconn = os.path.join(args.datadir, "conte69.restricted300.dconn.nii")
    run([args.wb_command, "-cifti-correlation", restricted, dconn])
    pconn = os.path.join(args.datadir, "conte69.ptseries.corr.pconn.nii")
    run([args.wb_command, "-cifti-correlation", ptseries, pconn])

    # 4. Report what was created, by reading the files with the package.
    print("\nCreated:")
    for path in (restricted, dconn, pconn):
        print("  %-44s %d bytes" % (os.path.basename(path), os.path.getsize(path)))


if __name__ == "__main__":
    main()
