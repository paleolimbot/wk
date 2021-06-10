
test_that("the wk_writer() generic resolves correct handler", {
  expect_s3_class(wk_writer(wkt()), "wk_wkt_writer")
  expect_s3_class(wk_writer(wkb()), "wk_wkb_writer")
  expect_s3_class(wk_writer(xy()), "wk_xy_writer")
  expect_s3_class(wk_writer(xy(), generic = TRUE), "wk_wkb_writer")
  expect_s3_class(wk_writer(rct()), "wk_wkb_writer")
  expect_s3_class(wk_writer(crc()), "wk_wkb_writer")
  expect_s3_class(wk_writer(structure(list(), class = "sfc")), "wk_sfc_writer")
})
