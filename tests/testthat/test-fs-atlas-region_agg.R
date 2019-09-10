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


