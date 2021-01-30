
test_that("wk_handler class works", {
  expect_true(is_wk_handler(wk_void_handler()))
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_output(print(wk_void_handler()), "wk_void_handler")
  expect_is(as_wk_handler(wk_void_handler), "wk_void_handler")
})

test_that("wk_handle.wk_xy() works", {
  expect_identical(
    wk_handle(xy(c(NA, 2, 3, NA), c(NA, NA, 4, 5)), wkt_writer()),
    c("POINT EMPTY", "POINT (2 nan)", "POINT (3 4)", "POINT (nan 5)")
  )

  expect_identical(
    wk_handle(xyz(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT Z (2 nan nan)", "POINT Z (3 4 nan)", "POINT Z (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xym(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT M (2 nan nan)", "POINT M (3 4 nan)", "POINT M (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xyzm(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA), c(NA, rep(1, 3))), wkt_writer()),
    c("POINT EMPTY", "POINT ZM (2 nan nan 1)", "POINT ZM (3 4 nan 1)", "POINT ZM (nan 5 nan 1)")
  )
})

test_that("wk_handle.wk_rct() works", {
  expect_identical(
    wk_handle(rct(c(1, NA, Inf, 0), c(2, NA, 0, Inf), c(3, NA, 1, 1), c(4, NA, 1, 1)), wkt_writer()),
    c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", "POLYGON EMPTY", "POLYGON EMPTY", "POLYGON EMPTY")
  )
})
