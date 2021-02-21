
test_that("the wk_writer() generic resolves correct handler", {
  expect_is(wk_writer(wkt()), "wk_wkt_writer")
  expect_is(wk_writer(wkb()), "wk_wkb_writer")
  expect_is(wk_writer(xy()), "wk_xy_writer")
  expect_is(wk_writer(structure(list(), class = "sfc")), "wk_sfc_writer")
})
