
test_that("wk_count() works", {
  expect_identical(
    wk_count(wkt(c("POINT (1 2)", "POLYGON ((0 0, 0 1, 1 1, 0 0))", NA))),
    data.frame(
      n_geom = c(1L, 1L, 0L),
      n_ring = c(0L, 1L, 0L),
      n_coord = c(1, 4L, 0)
    )
  )
})

test_that("wk_count() works for a vector of indeterminate length", {
  long_xy <- as_wkt(xy(runif(2048), runif(2048)))
  expect_identical(
    new_data_frame(wk_cpp_handle_wkt(long_xy, wk_count_handler(), reveal_size = FALSE)),
    wk_count(long_xy)
  )
})
