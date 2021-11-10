
test_that("wk_slice() works", {
  expect_identical(
    wk_slice(xy(1:5, 1:5), 3, 6),
    xy(3:5, 3:5)
  )
  expect_identical(
    wk_slice(xy(1:5, 1:5), 3, 6, times = 3, each = 2),
    rep(xy(rep(3:5, each = 2), rep(3:5, each = 2)), times = 3)
  )
  expect_identical(
    wk_slice(xy(1:5, 1:5), 0, 2),
    xy(1:2, 1:2)
  )
  expect_identical(
    wk_slice(xy(1:5, 1:5), 5, 4),
    xy(crs = NULL)
  )
})
