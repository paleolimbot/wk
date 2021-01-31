
test_that("wk_handle() works for data.frame", {
  expect_error(wk_handle(data.frame(a = 1)), "must have exactly one")
  expect_identical(
    wk_handle(data.frame(a = wkt("POINT (0 1)")), wkb_writer()),
    wk_handle(wkt("POINT (0 1)"), wkb_writer())
  )
})
