test_that("Our demo annotation file can be read", {
    annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
    annot = read.fs.annot(annotfile);
    known_vertex_count = 149244;

    color_table = colortable.from.annot(annot);

    # Test that the number of entries is correct, and that metadata matches data
    expect_equal(annot$colortable$num_entries, 36);
    expect_equal(length(annot$colortable$struct_names), 36)

    # Test that structure names are correct
    expect_equal(annot$colortable$struct_names[1], "unknown");
    expect_equal(annot$colortable$struct_names[2], "bankssts");
    expect_equal(annot$colortable$struct_names[3], "caudalanteriorcingulate");
    expect_equal(annot$colortable$struct_names[4], "caudalmiddlefrontal");
    expect_equal(annot$colortable$struct_names[5], "corpuscallosum");
    expect_equal(annot$colortable$struct_names[6], "cuneus");
    expect_equal(annot$colortable$struct_names[33], "frontalpole");
    expect_equal(annot$colortable$struct_names[34], "temporalpole");
    expect_equal(annot$colortable$struct_names[35], "transversetemporal");
    expect_equal(annot$colortable$struct_names[36], "insula");
    expect_equal(ncol(annot$colortable$table), 5);
    expect_equal(nrow(annot$colortable$table), 36);

    # Test RGB color for 'unknown'
    expect_equal(annot$colortable$table[1,1:3], c(25,5,25));

    # Test RGBA color for 'bankssts'
    expect_equal(annot$colortable$table[2,1:4], c(25,100,40,0));

    # Test the unique code for 'insula'
    expect_equal(annot$colortable$table[36,5], 2146559);

    # Test number of vertices and labels
    expect_equal(length(annot$vertices), known_vertex_count);
    expect_equal(length(annot$label_codes), known_vertex_count);

    # Test vertex indices: vertex indices in the file are zero-based
    expect_equal(annot$vertices[1], 0);
    for (vert_idx in c(15, 500, 100000)) {
      expect_equal(annot$vertices[vert_idx], vert_idx-1);
    }

    # Test vertex labels
    expect_equal(annot$label_codes[1], 9182740);
    expect_equal(annot$label_names[1], "lateraloccipital");
    expect_equal(length(annot$label_names), known_vertex_count);

    # Test region data
    thicknessfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE);
    thickness = read.fs.morph(thicknessfile);
    thickness_in_bankssts = thickness[annot$label_names == "bankssts"];
    expect_equal(length(thickness_in_bankssts), 1722);
    expect_equal(mean(thickness_in_bankssts), 2.49, tolerance=1e-2);

    # Test colortable_df
    cdf = annot$colortable_df;
    expect_equal(class(cdf), "data.frame");
    expect_equal(nrow(cdf), 36);
    expect_equal(ncol(cdf), 9);
    column_names = colnames(cdf);
    expect_true("struct_name" %in% column_names);
    expect_true("r" %in% column_names);
    expect_true("g" %in% column_names);
    expect_true("b" %in% column_names);
    expect_true("a" %in% column_names);
    expect_true("code" %in% column_names);
    expect_true("hex_color_string_rgb" %in% column_names);
    expect_true("hex_color_string_rgba" %in% column_names);

    expect_equal(typeof(annot$hex_colors_rgb), "character");
    expect_equal(length(annot$hex_colors_rgb), known_vertex_count);
 })


test_that("Annotation files in old format can be read", {

  skip_if(tests_running_on_cran_under_macos(), message = "Skipping on CRAN under MacOS, required test data cannot be downloaded.");
  freesurferformats::download_opt_data();
  subjects_dir = freesurferformats::get_opt_data_filepath("subjects_dir");
  annotfile = file.path(subjects_dir, "subject1", "label", "lh.aparc.a2005s.annot");
  skip_if_not(file.exists(annotfile), message="Test data missing.") ;

  known_vertex_count = 149244;

  annot = read.fs.annot(annotfile);

  expect_true(is.fs.annot(annot));

  # Test that the number of entries is correct, and that metadata matches data
  expect_equal(annot$colortable$num_entries, 82);
  expect_equal(length(annot$colortable$struct_names), 82);

  # Test that structure names are correct
  expect_equal(annot$colortable$struct_names[1], "Unknown");
  expect_equal(annot$colortable$struct_names[82], "S_temporal_transverse");

  expect_equal(typeof(annot$hex_colors_rgb), "character");
  expect_equal(length(annot$hex_colors_rgb), known_vertex_count);
})


test_that("A color lookup table (LUT) in ASCII format can be read", {
  lutfile = system.file("extdata", "colorlut.txt", package = "freesurferformats", mustWork = TRUE);

  # Test reading with computation of color code
  colortable = read.fs.colortable(lutfile, compute_colorcode=TRUE);

  expect_equal(nrow(colortable), 6);
  expect_equal(ncol(colortable), 7);
  expect_equal(colnames(colortable), c("struct_index", "struct_name", "r", "g", "b", "a", "code"));
  expect_equal(colortable$struct_index, seq(0, 5));
  expect_equal(colortable$r, c(0L, 70L, 245L, 205L, 120L, 196L));


  # Test again without computation of color code
  colortable = read.fs.colortable(lutfile);

  expect_equal(nrow(colortable), 6);
  expect_equal(ncol(colortable), 6);
  expect_equal(colnames(colortable), c("struct_index", "struct_name", "r", "g", "b", "a"));
  expect_equal(colortable$struct_index, seq(0, 5));
  expect_equal(colortable$r, c(0L, 70L, 245L, 205L, 120L, 196L));
})


test_that("A color lookup table can be extracted from an annotation", {
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);
  color_table = colortable.from.annot(annot, compute_colorcode = TRUE);

  # fill struct_index and re-read
  annot$colortable_df$struct_index = seq(0L, annot$colortable$num_entries - 1L);
  color_table = colortable.from.annot(annot);


  # expected errors should be raised
  annot_broken = annot;
  annot_broken$colortable$table = NULL;
  expect_error(colortable.from.annot(annot_broken));

})


test_that("An annotation can be read in GIFTI format.", {
  # we first write the annot file.
  annotfile = system.file("extdata", "lh.aparc.annot.gz", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annotfile);

  output_file = tempfile(fileext = ".annot.gii");
  write.fs.annot.gii(output_file, annot);

  # now read it
  annot2 = read.fs.annot.gii(output_file);

  expect_equal(annot$vertices, annot2$vertices, tolerance = 1e-2);
  expect_equal(annot$label_codes, annot2$label_codes);
  expect_equal(annot$label_names, annot2$label_names);
  expect_equal(annot$hex_colors_rgb, annot2$hex_colors_rgb);

  annot2_lables_only = read.fs.annot.gii(output_file, labels_only = TRUE);

  # check for expected errors
  testthat::expect_error(read.fs.annot.gii(output_file, rgb_column_names = c('Red', 'Green'))); # only 2 color columns given, 4 required
  testthat::expect_error(read.fs.annot.gii(output_file, key_column_name = "no_such_column"));  # key column does not exist
  testthat::expect_warning(read.fs.annot.gii(output_file, rgb_column_names = c('Red', 'Green', 'nosuchcolor', 'nope'))); # 4 color columns given, but 2 do not occur in file
})



test_that("An file in FreeSurfer GCA format can be read.", {
  fs_home = Sys.getenv('FREESURFER_HOME');
  gca_file = file.path(fs_home, 'average', 'face.gca');
  skip_if_not(file.exists(gca_file), message="Freesurfer installation with GCA file available.");

  gca = read.fs.gca(gca_file);
  testthat::expect_equal(gca$gca_version, 4L);
  testthat::expect_equal(gca$prior_spacing, 2L);
})


