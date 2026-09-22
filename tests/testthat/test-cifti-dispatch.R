# Tests for the CIFTI-2 detection in the generic format dispatchers (item I.2e).
#
# A CIFTI-2 file is a NIFTI-2 file, so the dispatchers of this package used to interpret
# its payload as a volume or as a per-vertex morphometry vector. Both are silent wrong
# results (the payload is a matrix whose dimensions the XML describes), so the dispatchers
# now refuse such files with a pointer to the CIFTI readers, and the non-CIFTI writers
# refuse a CIFTI-2 file name. These tests check both directions, including the cases that
# must NOT be refused.

cifti.test.all.fixtures <- function() {
  return(list.files(system.file("extdata", "cifti", package = "freesurferformats"),
                    pattern = "[.]nii$", full.names = TRUE))
}

test_that("CIFTI-2 files are detected", {
  fixtures <- cifti.test.all.fixtures()
  expect_length(fixtures, 13L)
  for (fixture in fixtures) {
    expect_true(cifti.file.looks.like.cifti2(fixture), info = basename(fixture))
  }
  # Plain NIFTI files are not.
  expect_false(cifti.file.looks.like.cifti2(system.file("extdata", "tiny.nii", package = "freesurferformats")))
  expect_false(cifti.file.looks.like.cifti2(system.file("extdata", "vol27int.nii.gz", package = "freesurferformats")))
  expect_false(cifti.file.looks.like.cifti2(system.file("extdata", "lh.thickness", package = "freesurferformats")))
  expect_false(cifti.file.looks.like.cifti2("/does/not/exist.nii"))
  expect_false(cifti.file.looks.like.cifti2(NULL))
  expect_false(cifti.file.looks.like.cifti2(42L))

  # A NIFTI-2 file without the CIFTI extension is not a CIFTI-2 file.
  ni2_file <- file.path(tempdir(), "plain_nifti2.nii")
  write.nifti2(ni2_file, array(as.numeric(1:8), dim = c(2L, 2L, 2L)), ni2header.template())
  expect_false(cifti.file.looks.like.cifti2(ni2_file))
  unlink(ni2_file)
})

test_that("The morphometry dispatcher refuses CIFTI-2 files", {
  for (fixture in cifti.test.all.fixtures()) {
    expect_error(read.fs.morph(fixture), "is a CIFTI-2 file, not a NIFTI volume or morphometry file")
    # Also when the format is given explicitly: a CIFTI file is not a NIFTI morph file.
    expect_error(read.fs.morph(fixture, format = "ni2"), "is a CIFTI-2 file")
  }
  # The error names the readers that can be used instead.
  expect_error(read.fs.morph(cifti.test.all.fixtures()[1L]), "read.fs.morph.cifti\\(\\)")
  expect_error(read.fs.morph(cifti.test.all.fixtures()[1L]), "read.cifti\\(\\)")
  expect_error(read.fs.morph(cifti.test.all.fixtures()[1L]), "read.fs.connectome.cifti\\(\\)")
})

test_that("The volume dispatcher refuses CIFTI-2 files", {
  for (fixture in cifti.test.all.fixtures()) {
    expect_error(read.fs.volume(fixture), "is a CIFTI-2 file, not a NIFTI volume or morphometry file")
  }
  expect_error(read.fs.volume(cifti.test.all.fixtures()[1L]), "read.cifti\\(\\)")
})

test_that("The dispatchers still read plain NIFTI files", {
  # A NIFTI-1 volume.
  expect_equal(dim(read.fs.volume(system.file("extdata", "vol27int.nii.gz", package = "freesurferformats"))),
               c(3L, 3L, 3L, 1L))
  # A NIFTI morphometry vector (tiny.nii holds a per-vertex vector).
  morph <- read.fs.morph(system.file("extdata", "tiny.nii", package = "freesurferformats"))
  expect_true(is.numeric(morph))
  expect_true(length(morph) > 0L)
  expect_true(is.numeric(read.fs.morph(system.file("extdata", "lh.thickness", package = "freesurferformats"))))
  expect_true(is.numeric(read.fs.morph(system.file("extdata", "tiny.mgh", package = "freesurferformats"))))
})

test_that("A NIFTI file that only mentions CIFTI is not refused", {
  # The CIFTI-1 detector looks for the string 'CIFTI' in the first bytes of the file, which
  # a NIFTI file could contain by accident (e.g. in its description field). Only files with
  # a CIFTI intent code are refused, so such a file has to be read normally.
  data <- array(as.numeric(1:8), dim = c(2L, 2L, 2L))
  nii_file <- file.path(tempdir(), "mentions_cifti.nii")
  niiheader <- ni1header.for.data(data)
  niiheader$descrip <- "created with CIFTI tools"
  write.nifti1(nii_file, data, niiheader = niiheader)
  expect_true(cifti.file.looks.like.cifti1(nii_file))
  expect_silent(cifti.stop.if.cifti(nii_file))
  expect_equal(length(read.fs.morph(nii_file)), 8L)
  unlink(nii_file)
})

test_that("A CIFTI-1 file is refused with a conversion hint", {
  data <- array(as.numeric(1:8), dim = c(2L, 2L, 2L))
  ni1_file <- file.path(tempdir(), "cifti1_like.nii")
  ni1header <- ni1header.for.data(data)
  ni1header$intent_code <- 3001L
  ni1header$descrip <- "CIFTI"
  write.nifti1(ni1_file, data, niiheader = ni1header)
  expect_error(read.fs.morph(ni1_file), "looks like a CIFTI-1 file")
  expect_error(read.fs.volume(ni1_file), "-version-convert")
  unlink(ni1_file)
})

test_that("The non-CIFTI writers refuse CIFTI-2 file names", {
  cifti_names <- c("x.dscalar.nii", "x.dlabel.nii", "x.dtseries.nii", "x.dconn.nii", "x.pscalar.nii",
                   "x.ptseries.nii", "x.pconn.nii", "x.dpconn.nii", "x.pdconn.nii")
  for (file_name in cifti_names) {
    target <- file.path(tempdir(), file_name)
    expect_error(write.fs.morph(target, 1:10), "is the name of a CIFTI-2 file type", info = file_name)
    expect_false(file.exists(target))
  }
  volume <- read.fs.volume(system.file("extdata", "brain.mgz", package = "freesurferformats"), with_header = TRUE)
  expect_error(write.fs.volume(file.path(tempdir(), "x.dscalar.nii"), volume), "write.cifti\\(\\)")

  # Ordinary names still work, and the file names of the CIFTI writers are not affected.
  expect_equal(write.fs.morph(file.path(tempdir(), "ok.nii"), 1:10), "nii")
  write.fs.volume(file.path(tempdir(), "ok_volume.nii"), volume)
  expect_true(file.exists(file.path(tempdir(), "ok_volume.nii")))
  out_file <- file.path(tempdir(), "written.dscalar.nii")
  template <- system.file("extdata", "cifti", "tiny.dscalar.nii", package = "freesurferformats")
  expect_silent(write.cifti(out_file, read.cifti(template)$data, template = template))
  expect_true(file.exists(out_file))
  unlink(c(file.path(tempdir(), "ok.nii"), file.path(tempdir(), "ok_volume.nii"), out_file))
})
