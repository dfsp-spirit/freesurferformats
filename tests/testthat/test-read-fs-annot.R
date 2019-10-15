test_that("Our demo annotation file can be read", {
    annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE)
    annot = read.fs.annot(annotfile)
    known_vertex_count = 149244

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(annot$colortable$num_entries, 36);
    expect_equal(length(annot$colortable$struct_names), 36)

    # Test that structure names are correct
    expect_equal(annot$colortable$struct_names[1], "unknown")
    expect_equal(annot$colortable$struct_names[2], "bankssts")
    expect_equal(annot$colortable$struct_names[3], "caudalanteriorcingulate")
    expect_equal(annot$colortable$struct_names[4], "caudalmiddlefrontal")
    expect_equal(annot$colortable$struct_names[5], "corpuscallosum")
    expect_equal(annot$colortable$struct_names[6], "cuneus")
    expect_equal(annot$colortable$struct_names[33], "frontalpole")
    expect_equal(annot$colortable$struct_names[34], "temporalpole")
    expect_equal(annot$colortable$struct_names[35], "transversetemporal")
    expect_equal(annot$colortable$struct_names[36], "insula")
    expect_equal(ncol(annot$colortable$table), 5)
    expect_equal(nrow(annot$colortable$table), 36)

    # Test RGB color for 'unknown'
    expect_equal(annot$colortable$table[1,1:3], c(25,5,25))

    # Test RGBA color for 'bankssts'
    expect_equal(annot$colortable$table[2,1:4], c(25,100,40,0))

    # Test the unique code for 'insula'
    expect_equal(annot$colortable$table[36,5], 2146559)

    # Test number of vertices and labels
    expect_equal(length(annot$vertices), known_vertex_count)
    expect_equal(length(annot$label_codes), known_vertex_count)

    # Test vertex indices: vertex indices in the file are zero-based
    expect_equal(annot$vertices[1], 0)
    for (vert_idx in c(15, 500, 100000)) {
      expect_equal(annot$vertices[vert_idx], vert_idx-1)
    }

    # Test vertex labels
    expect_equal(annot$label_codes[1], 9182740)
    expect_equal(annot$label_names[1], "lateraloccipital")
    expect_equal(length(annot$label_names), known_vertex_count)

    # Test region data
    thicknessfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE)
    thickness = read.fs.morph(thicknessfile)
    thickness_in_bankssts = thickness[annot$label_names == "bankssts"]
    expect_equal(length(thickness_in_bankssts), 1722)
    expect_equal(mean(thickness_in_bankssts), 2.49, tolerance=1e-2)

    # Test colortable_df
    cdf = annot$colortable_df;
    expect_equal(class(cdf), "data.frame");
    expect_equal(nrow(cdf), 36);
    expect_equal(ncol(cdf), 8);
    column_names = colnames(cdf);
    expect_true("struct_name" %in% column_names);
    expect_true("r" %in% column_names);
    expect_true("g" %in% column_names);
    expect_true("b" %in% column_names);
    expect_true("a" %in% column_names);
    expect_true("code" %in% column_names);
    expect_true("hex_color_string_rgb" %in% column_names);
    expect_true("hex_color_string_rgba" %in% column_names);

    expect_equal(typeof(annot$hex_colors_rgb), "character")
    expect_equal(length(annot$hex_colors_rgb), known_vertex_count)
 })

test_that("Annotation files in old format can be read", {
  annotfile = system.file("extdata", "lh.aparc.a2005s.annot", package = "freesurferformats", mustWork = TRUE)
  annot = read.fs.annot(annotfile)
  known_vertex_count = 163842

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(annot$colortable$num_entries, 82);
  expect_equal(length(annot$colortable$struct_names), 82)

  # Test that structure names are correct
  expect_equal(annot$colortable$struct_names[1], "Unknown")
  expect_equal(annot$colortable$struct_names[2], "Corpus_callosum")

  expect_equal(typeof(annot$hex_colors_rgb), "character")
  expect_equal(length(annot$hex_colors_rgb), known_vertex_count)
})



