test_that("A brain atlas can be constructed from a LUT and per-vertex label indices.", {
  # Tiny LUT with left/right regions sharing a color (like the Desikan-Killiany 'aparc' atlas).
  colortable_df <- data.frame(
    "struct_index" = c(1L, 2L, 3L, 4L),
    "struct_name" = c("L_regionA", "L_regionB", "R_regionA", "R_regionB"),
    "r" = c(10L, 20L, 10L, 20L), # L and R regions share colors
    "g" = c(10L, 20L, 10L, 20L),
    "b" = c(10L, 20L, 10L, 20L),
    "a" = c(0L, 0L, 0L, 0L),
    stringsAsFactors = FALSE
  )
  lut_file <- tempfile(fileext = ".txt")
  write.fs.colortable(lut_file, colortable_df)

  # Per-vertex label indices: one unlabeled vertex (0) plus one vertex per region.
  label_indices <- c(0L, 1L, 2L, 3L, 4L, 1L)
  annot <- atlas.from.lut.and.csv(lut_file, label_indices = label_indices)

  expect_true(is.fs.annot(annot))
  expect_equal(length(annot$vertices), 6)
  expect_equal(annot$vertices, 0:5)

  # An 'unknown' entry is prepended to the colortable (struct index 0).
  expect_equal(annot$colortable$num_entries, 5)
  expect_equal(annot$colortable$struct_names[1], "unknown")
  expect_equal(annot$colortable$struct_index[1], 0)

  # Per-vertex names are derived from the struct index, NOT from the (shared) color code.
  expect_equal(annot$label_names, c("unknown", "L_regionA", "L_regionB", "R_regionA", "R_regionB", "L_regionA"))
  # Unlabeled vertices get the 'unknown' color code 0.
  expect_equal(annot$label_codes[1], 0)
  expect_equal(annot$label_codes[2], annot$colortable_df$code[2])
})


test_that("An annotation can be written to a LUT and per-vertex label file and read back losslessly.", {
  annotfile <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE)
  annot <- read.fs.annot(annotfile, default_label_name = "unknown")

  lut_file <- tempfile(fileext = ".txt")
  csv_file <- tempfile(fileext = ".csv")
  res <- write.atlas.to.lut.and.csv(annot, lut_file, csv_file)

  expect_equal(res$num_vertices, length(annot$label_codes))
  expect_equal(res$num_regions, nrow(annot$colortable_df))

  annot2 <- atlas.from.lut.and.csv(lut_file, csv_file)

  expect_equal(annot$vertices, annot2$vertices)

  # All vertices that are assigned to a real region keep their exact region name.
  labelled <- annot$label_names != ""
  expect_equal(annot2$label_names[labelled], annot$label_names[labelled])

  # Unlabelled vertices (code 0, not in the colortable) are exported as struct index 0
  # and read back as the 'unknown' region.
  expect_true(all(annot2$label_names[!labelled] == "unknown"))

  # The colortable is preserved exactly.
  expect_equal(annot$colortable_df, annot2$colortable_df)
})


test_that("The round trip via atlas.from.lut.and.csv -> write.atlas.to.lut.and.csv is lossless.", {
  # An annotation created from a LUT + labels stores the per-vertex label indices in its
  # metadata, which write.atlas.to.lut.and.csv uses to make the export exact even when
  # several regions share a color code.
  colortable_df <- data.frame(
    "struct_index" = c(1L, 2L),
    "struct_name" = c("L_regionA", "R_regionA"),
    "r" = c(10L, 10L), # shared color
    "g" = c(10L, 10L),
    "b" = c(10L, 10L),
    "a" = c(0L, 0L),
    stringsAsFactors = FALSE
  )
  lut_file <- tempfile(fileext = ".txt")
  write.fs.colortable(lut_file, colortable_df)

  label_indices <- c(0L, 1L, 2L, 1L, 2L)
  annot <- atlas.from.lut.and.csv(lut_file, label_indices = label_indices)

  out_lut <- tempfile(fileext = ".txt")
  out_csv <- tempfile(fileext = ".csv")
  write.atlas.to.lut.and.csv(annot, out_lut, out_csv)

  annot2 <- atlas.from.lut.and.csv(out_lut, out_csv)
  expect_equal(annot$label_codes, annot2$label_codes)
  expect_equal(annot$label_names, annot2$label_names)
  expect_equal(annot$colortable_df, annot2$colortable_df)
})


test_that("atlas.from.lut.and.csv validates its inputs.", {
  expect_error(atlas.from.lut.and.csv("/nonexistent/file.lut")) # LUT file does not exist

  colortable_df <- data.frame(struct_index = 0L, struct_name = "x", r = 0L, g = 0L, b = 0L, a = 0L)
  lut_file <- tempfile(fileext = ".txt")
  write.fs.colortable(lut_file, colortable_df)

  expect_error(atlas.from.lut.and.csv(lut_file)) # neither csv_file nor label_indices given
  expect_error(atlas.from.lut.and.csv(lut_file, label_indices = c(0L, 0L), num_vertices = 5L)) # num_vertices mismatch
})


test_that("write.atlas.to.lut.and.csv validates its inputs.", {
  expect_error(write.atlas.to.lut.and.csv(list("a" = 1), tempfile(fileext = ".txt"), tempfile(fileext = ".csv"))) # not an fs.annot

  annotfile <- system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE)
  annot <- read.fs.annot(annotfile)
  annot$colortable_df <- NULL # remove the colortable
  expect_error(write.atlas.to.lut.and.csv(annot, tempfile(fileext = ".txt"), tempfile(fileext = ".csv"))) # missing colortable
})
