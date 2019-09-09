test_that("Our demo annotation file can be read", {
    annotfile = system.file("extdata", "lh.aparc.annot", package = "freesurferformats", mustWork = TRUE)
    annot = read.fs.annot(annotfile)
    known_vertex_count = 149244

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(annot$colortable$num_entries, 36);
    expect_equal(length(annot$colortable$struct_names), 36)

    # Test that structure names are correct
    expect_equal(annot$colortable$struct_names[1], "unknown")
    expect_equal(annot$colortable$struct_names[2], "bankssts")
    expect_equal(annot$colortable$struct_names[3], "caudalanteriorcingulate")
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


})
