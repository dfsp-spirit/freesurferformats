# Neutralize the provenance metadata of the CIFTI-2 files this repository generates.
#
# Connectome Workbench stores the command line it was called with in the CIFTI-2 metadata
# ('Provenance', 'ParentProvenance', 'WorkingDirectory'), including absolute paths of the
# machine and the directory the generator ran in. That is useful while debugging, but these
# files are committed, and the paths differ on every machine, so the same generator run on
# another machine would produce a different file.
#
# This script rewrites the CIFTI-2 header extension of such a file (with the package's own
# writer, see write.nifti2() and cifti.header.from.axes()) so that the metadata states what
# was done without naming a directory of a developer's machine: every absolute path in a
# provenance entry is reduced to the name of the file it points at (the Workbench binary
# becomes 'wb_command'), and 'WorkingDirectory' (which describes the developer's checkout) is
# dropped. The data, the axes, the intent code and the rest of the metadata are unchanged,
# which the script verifies, and dev_tools/check_cifti_conversion.R proves afterwards that
# Connectome Workbench and nibabel still read the files (they compare their view of the file
# with ours, so a broken rewrite cannot pass).
#
# The official example files that are kept in extra_test_data (the Conte69 files of the HCP)
# are *not* rewritten by default: they are third party reference data, and their provenance
# documents where they came from. Only run this script on them if the paths have to go.
#
# Usage:
#
#   Rscript dev_tools/neutralize_cifti_provenance.R [--dir DIR] [--check]
#
# Without arguments the shipped fixtures in inst/extdata/cifti are rewritten (in place, so
# run it in a clean working tree). Pass '--dir extra_test_data/cifti' to process the test
# data of the repository that this package generates (the connectome files). --check only
# reports what would change.

suppressMessages(devtools::load_all("/home/ts/develop/freesurferformats", quiet = TRUE))

args <- commandArgs(trailingOnly = TRUE)
repo_dir <- "/home/ts/develop/freesurferformats"
target_dir <- file.path(repo_dir, "inst", "extdata", "cifti")
check_only <- "--check" %in% args
dir_arg <- which(args == "--dir")
if (length(dir_arg) > 0L && dir_arg < length(args)) {
  target_dir <- args[dir_arg + 1L]
}

# Replace the machine specific parts of one metadata entry.
#
# The provenance entries hold command lines ('Provenance', 'ParentProvenance') that name the
# files and the binary they used, always as absolute paths of the machine that ran them. The
# file names are what documents how a file was made, so every absolute path is reduced to the
# name of the file it points at: the command then reads as it would have been typed in the
# directory the generator works in, on any machine.
neutralize_provenance_text <- function(text) {
  if (is.null(text) || is.na(text)) {
    return(text)
  }
  return(gsub("(?:/[^ \n:/]+)*/([^ \n:/]+)", "\\1", text, perl = TRUE))
}

# The metadata of one file, with the provenance neutralized. 'WorkingDirectory' describes
# the checkout of the developer and is dropped. The other entries (e.g. the program
# provenance, which documents the Workbench build, or the palette mappings of the maps) are
# not touched.
neutralized_metadata <- function(metadata) {
  provenance_entries <- grepl("Provenance$", names(metadata))
  metadata[provenance_entries] <- lapply(metadata[provenance_entries], neutralize_provenance_text)
  metadata <- metadata[names(metadata) != "WorkingDirectory"]
  return(metadata)
}

# Check that no absolute path of a machine is left in the metadata. The files should be
# reproducible on any machine, so a new kind of path is a reason to fail loudly here.
assert_no_machine_paths <- function(metadata, filepath) {
  paths <- grep("(^|[\"' ])/[a-zA-Z]", unlist(metadata, use.names = FALSE), value = TRUE)
  # The program provenance names the directory of the compiler ('/usr/bin'), which is not
  # machine specific in the sense of this script.
  paths <- paths[!grepl("/usr/", paths)]
  if (length(paths) > 0L) {
    stop(sprintf("File '%s' still contains absolute paths after neutralization:\n  %s\n",
                 basename(filepath), paste(utils::head(paths, 5L), collapse = "\n  ")))
  }
  return(invisible(NULL))
}

files <- list.files(target_dir, pattern = "[.]nii$", full.names = TRUE)
failures <- 0L
for (filepath in files) {
  cii <- read.cifti.header(filepath)
  data <- read.cifti(filepath)$data
  axes <- cifti.axis.from.template(cii)
  metadata <- neutralized_metadata(cii$matrix$metadata)

  changed <- !isTRUE(all.equal(metadata, cii$matrix$metadata))
  cat(sprintf("%-28s %s\n", basename(filepath), if (changed) "provenance neutralized" else "unchanged"))
  if (check_only || !changed) {
    next
  }

  xml <- cifti.header.from.axes(axes, metadata = metadata)
  niiheader <- cii$niiheader
  niiheader$extensions <- NULL # they are passed separately, see write.nifti2()
  write.nifti2(filepath, data, niiheader, extensions = list(nifti2.extension(CIFTI_EXTENSION_CODE, xml)))

  # Verify the rewrite: the data and the axes have to be the ones of the file before, and
  # the metadata has to be free of machine paths.
  reread <- read.cifti(filepath)
  if (!isTRUE(all.equal(unname(reread$data), unname(data)))) {
    failures <- failures + 1L
    cat(sprintf("  FAIL: the data changed when rewriting %s\n", basename(filepath)))
  }
  if (!isTRUE(all.equal(reread$header$matrix$indices_maps, cii$matrix$indices_maps))) {
    failures <- failures + 1L
    cat(sprintf("  FAIL: the axes changed when rewriting %s\n", basename(filepath)))
  }
  if (!isTRUE(all.equal(neutralized_metadata(reread$header$matrix$metadata), metadata))) {
    failures <- failures + 1L
    cat(sprintf("  FAIL: the metadata changed when rewriting %s\n", basename(filepath)))
  }
  if (!identical(reread$header$niiheader$intent_code, cii$niiheader$intent_code)) {
    failures <- failures + 1L
    cat(sprintf("  FAIL: the intent code changed when rewriting %s\n", basename(filepath)))
  }
  assert_no_machine_paths(reread$header$matrix$metadata, filepath)
}

if (check_only) {
  cat(sprintf("\n%d files would be rewritten (--check, nothing was written).\n", length(files)))
} else {
  cat(sprintf("\n%d of %d files rewritten, %d failures.\n", sum(files != ""), length(files), failures))
}
quit(status = if (failures == 0L) 0L else 1L)
