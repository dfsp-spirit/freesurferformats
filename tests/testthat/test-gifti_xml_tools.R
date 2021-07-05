

test_that("We can add global metadata to a GIFTI xml tree", {
  testthat::skip_on_cran();
  testthat::skip_if_offline(host = "www.nitrc.org");
  xmltree = gifti_xml(list(rep(3.1, 3L), matrix(seq(6)+0.1, nrow=2L)));
  newtree = gifti_xml_add_global_metadata(xmltree, list("User"="Me", "Weather"="Great"));
  gifti_xsd = "https://www.nitrc.org/frs/download.php/158/gifti.xsd";
  xml_is_valid = xml2::xml_validate(newtree, xml2::read_xml(gifti_xsd));

  expect_true(xml_is_valid);
})


test_that("We can generate a GIFTI transform matrix node", {
  tf_matrix = diag(4);
  tf_node = xml_node_gifti_coordtransform(tf_matrix);

  expect_error(xml_node_gifti_coordtransform(tf_matrix, data_space = 3)); # space must be string
  expect_error(xml_node_gifti_coordtransform(tf_matrix, transformed_space = 3)); # same
  expect_error(xml_node_gifti_coordtransform(123)); # not a matrix
  expect_error(xml_node_gifti_coordtransform(diag(3))); # wrong matrix dim
})


test_that("CDATA encapsulation of strings works", {
  cdata_start_tag = "<![CDATA[";
  cdata_end_tag = "]]>";

  expect_error(cdata(cdata_start_tag));
  expect_error(cdata(cdata_end_tag));
})

