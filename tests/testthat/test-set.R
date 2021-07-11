
test_that("wk_set_(z|m)() works", {
  expect_identical(wk_set_z(wkt("POINT (0 1)"), 2), wkt("POINT Z (0 1 2)"))
  expect_identical(wk_set_m(wkt("POINT (0 1)"), 2), wkt("POINT M (0 1 2)"))
  expect_identical(wk_set_z(wkt("POINT M (0 1 3)"), 2), wkt("POINT ZM (0 1 2 3)"))
  expect_identical(wk_set_m(wkt("POINT Z (0 1 3)"), 2), wkt("POINT ZM (0 1 3 2)"))
  expect_identical(wk_set_z(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 7 3)"))
  expect_identical(wk_set_m(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 2 7)"))
})

test_that("wk_drop_(z|m) works", {
  expect_identical(wk_drop_z(wkt("POINT ZM (0 1 2 3)")), wkt("POINT M (0 1 3)"))
  expect_identical(wk_drop_m(wkt("POINT ZM (0 1 2 3)")), wkt("POINT Z (0 1 2)"))
})

test_that("wk_trans_set() is vectorized", {
  expect_identical(
    wk_handle(
      rep(wkt("POINT Z (0 0 0)"), 4),
      wk_transform_filter(wkt_writer(), wk_trans_set(xyz(NA, NA, c(1, 2)), use_z = TRUE))
    ),
    rep(wkt(c("POINT Z (0 0 1)", "POINT Z (0 0 2)")), 2)
  )
})

test_that("wk_trans_set() can set ZM values at the same time", {
  expect_identical(
    wk_handle(
      wkt("POINT (0 0)"),
      wk_transform_filter(
        wkt_writer(),
        wk_trans_set(xyzm(NA, NA, 1, 2), use_z = TRUE, use_m = TRUE)
      )
    ),
    wkt("POINT ZM (0 0 1 2)")
  )
})

test_that("wk_trans_set() can set XY values", {
  expect_identical(
    wk_handle(
      wkt("POINT Z (0 0 0)"),
      wk_transform_filter(wkt_writer(), wk_trans_set(xy(1, 2)))
    ),
    wkt("POINT Z (1 2 0)")
  )
})
