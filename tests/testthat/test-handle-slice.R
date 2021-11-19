
test_that("wk_handle_slice() works", {
  expect_identical(
    wk_handle_slice(xy(1:5, 1:5), xy_writer(), 3, 6),
    xy(3:5, 3:5)
  )
  expect_identical(
    wk_handle_slice(xy(1:5, 1:5), xy_writer(), 0, 2),
    xy(1:2, 1:2)
  )
  expect_identical(
    wk_handle_slice(xy(1:5, 1:5), xy_writer(), 5, 4),
    xy(crs = NULL)
  )
})
