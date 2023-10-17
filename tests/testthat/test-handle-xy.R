
test_that("wk_handle.wk_xy() works", {
  expect_identical(
    wk_handle(xy(c(NA, NaN, 2, 3, NA), c(NA, NaN, NA, 4, 5)), wkt_writer()),
    wkt(c(NA, "POINT EMPTY", "POINT (2 nan)", "POINT (3 4)", "POINT (nan 5)"))
  )

  expect_identical(
    wk_handle(
      xyz(c(NA, NaN, 2, 3, NA), c(NA, NaN, NA, 4, 5), c(NA, NaN, NA, NA, NA)),
      wkt_writer()
    ),
    wkt(c(NA, "POINT Z EMPTY", "POINT Z (2 nan nan)", "POINT Z (3 4 nan)", "POINT Z (nan 5 nan)"))
  )

  expect_identical(
    wk_handle(
      xym(c(NA, NaN, 2, 3, NA), c(NA, NaN, NA, 4, 5), c(NA, NaN, NA, NA, NA)),
      wkt_writer()
    ),
    wkt(c(NA, "POINT M EMPTY", "POINT M (2 nan nan)", "POINT M (3 4 nan)", "POINT M (nan 5 nan)"))
  )

  expect_identical(
    wk_handle(
      xyzm(c(NA, NaN, 2, 3, NA), c(NA, NaN, NA, 4, 5), c(NA, NaN, NA, NA, NA), c(NA, NaN, rep(1, 3))),
      wkt_writer()
    ),
    wkt(c(NA, "POINT ZM EMPTY", "POINT ZM (2 nan nan 1)", "POINT ZM (3 4 nan 1)", "POINT ZM (nan 5 nan 1)"))
  )
})
