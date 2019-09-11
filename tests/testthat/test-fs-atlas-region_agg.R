test_that("Aggregation on subject level works", {
  curvfile = system.file("extdata", "lh.thickness", package = "freesurferformats", mustWork = TRUE)
  ct = read.fs.curv(curvfile);

  annot_file = system.file("extdata", "lh.aparc.annot", package = "freesurferformats", mustWork = TRUE);
  annot = read.fs.annot(annot_file);
  expect_equal(length(ct), length(annot$label_names));  # ensure the data fits together.

  # Test with the default names and aggregation function (mean)
  agg = fs.atlas.region.agg(ct, annot$label_names);
  expect_equal(class(agg), "data.frame");
  expect_equal(nrow(agg), 35);
  expect_equal(ncol(agg), 2);
  expect_equal(colnames(agg), c("region", "aggregated"));
  expect_true("bankssts" %in% agg$region);
  mean_bankssts = subset(agg, region=="bankssts", select=aggregated, drop=TRUE);
  expect_equal(mean_bankssts, 2.49, tolerance=1e-2);


  # Test with custom region names and aggregation function
  agg2 = fs.atlas.region.agg(ct, annot$label_names, agg_fun = max, requested_label_names=c("bankssts", "nosuchregion"));
  expect_equal(class(agg2), "data.frame");
  expect_equal(nrow(agg2), 2);   # Only the 2 explicitely requested regions should occur
  expect_equal(ncol(agg2), 2);
  expect_equal(colnames(agg2), c("region", "aggregated"));
  expect_true("nosuchregion" %in% agg2$region);
  expect_true("bankssts" %in% agg2$region);
  max_bankssts = subset(agg2, region=="bankssts", select=aggregated, drop=TRUE);
  expect_equal(max_bankssts, 3.9, tolerance=1e-2);
  max_nosuchregion = subset(agg2, region=="nosuchregion", select=aggregated, drop=TRUE);
  expect_true(is.nan(max_nosuchregion));

  # Test with incorrect input data: mismatch between vertex count and label count
  expect_error(fs.atlas.region.agg(ct, annot$label_names[1:10]), "Counts must match");
})


test_that("Aggregation on group level works", {
    subjects_dir = path.expand("~/data/tim_only")
    skip_if_not(dir.exists(subjects_dir), message="Test data missing.") # skip on travis
    subjects_list = c("tim", "timcopy")
    measure = "thickness"
    hemi = "lh"
    atlas = "aparc"

    # Test for mean aggregation
    agg.res = fs.atlas.region.agg.group(subjects_dir, subjects_list, measure, hemi, atlas);

    expect_equal(nrow(agg.res), 2);   # 2 subjects
    expect_equal(rownames(agg.res), c("tim", "timcopy"));
    expect_equal(ncol(agg.res), 36);  # 36 regions
    expect_true("bankssts" %in% colnames(agg.res));
    expect_equal(class(agg.res), "data.frame");

    mean_bankssts_tim = agg.res$bankssts[1]
    expect_equal(mean_bankssts_tim, 2.49, tolerance=1e-2);



    # Test for max aggregation
    agg.res = fs.atlas.region.agg.group(subjects_dir, subjects_list, measure, hemi, atlas, agg_fun = max);

    expect_equal(nrow(agg.res), 2);   # 2 subjects
    expect_equal(rownames(agg.res), c("tim", "timcopy"));
    expect_equal(ncol(agg.res), 36);  # 36 regions
    expect_true("bankssts" %in% colnames(agg.res));
    expect_equal(class(agg.res), "data.frame");

    max_bankssts_tim = agg.res$bankssts[1]
    expect_equal(max_bankssts_tim, 3.9, tolerance=1e-2);
})


