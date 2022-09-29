
test_that("grd_extract() works", {
  grid <- grd_rct(matrix(1:6, ncol = 3))
  expect_identical(
    grd_extract(grid, i = 2, j = 2:3),
    array(c(4L, 6L))
  )
})

test_that("grd_extract_nearest() works", {
  grid <- grd_rct(matrix(1:6, ncol = 3))
  expect_identical(
    grd_extract_nearest(grid, grd_cell_xy(grid, i = 2, j = 2:3)),
    array(c(4L, 6L))
  )
})

test_that("grd_data_extract() works for nativeRaster", {
  col_native <- structure(
    c(-16777216L, -13421773L, -10066330L, -15066598L, -11711155L, -8355712L),
    .Dim = 2:3,
    class = "nativeRaster"
  )

  data_21 <- grd_data_extract(col_native, i = 2, j = 2:3)
  expect_identical(
    data_21,
    array(c(-11711155L, -8355712L))
  )
})

test_that("grd_data_extract() works for matrix", {
  grid <- grd_rct(matrix(1:6, ncol = 3))
  expect_identical(
    grd_data_extract(grid$data, i = 2, j = 2:3),
    array(c(4L, 6L))
  )
})

test_that("grd_data_extract() works for matrix-like arrays", {
  grid <- grd_rct(array(1:6, dim = c(2L, 3L, 1L)))
  expect_identical(
    grd_data_extract(grid$data, i = 2, j = 2:3),
    array(c(4L, 6L), dim = c(2L, 1L))
  )
})

test_that("grd_data_extract() errors for bad data", {
  grid <- grd_rct(array(1:6, dim = c(2L, 3L, 4L)))
  expect_error(
    grd_data_extract(grid$data, i = 2, j = 2:3),
    "not implemented for non-matrix-like data"
  )
})
