
test_that("grd_xy() plot method works", {
  grid <- grd(nx = 3, ny = 2, type = "centers")
  expect_identical(
    plot(
      grid,
      col = rgb(0:5, 0:5, 0:5, maxColorValue = 5),
      pch = 16,
      cex = 5
    ),
    grid
  )
})

test_that("grd_rct() plot method works", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(plot(grid_empty, bbox = rct(0, 0, 1, 1)), grid_empty)

  grid_spec <- grd(nx = 3, ny = 2)
  expect_identical(plot(grid_spec, border = NULL), grid_spec)

  grid_numeric <- grd_rct(matrix(0:5, nrow = 2, ncol = 3))
  expect_identical(plot(grid_numeric), grid_numeric)

  grid_raster <- grd_rct(as.raster(grid_numeric$data / 10))
  expect_identical(plot(grid_raster), grid_raster)

  grid_raster_rev_y <- grd_rct(
    as.raster(grid_numeric$data / 10),
    bbox = rct(0, 0, 3, -2)
  )
  expect_identical(plot(grid_raster_rev_y), grid_raster_rev_y)

  grid_raster_rev_x <- grd_rct(
    as.raster(grid_numeric$data / 10),
    bbox = rct(0, 0, -3, 2)
  )
  expect_identical(plot(grid_raster_rev_x), grid_raster_rev_x)


  # can aso check with PNG
  # col_native <- png::readPNG(system.file("img", "Rlogo.png", package="png"), native = T)
  col_native <- structure(
    c(-16777216L, -13421773L, -10066330L, -15066598L, -11711155L, -8355712L),
    .Dim = 2:3,
    class = "nativeRaster"
  )

  grid_native <- grd_rct(col_native)
  expect_identical(plot(grid_native), grid_native)

  grid_native_rev_y <- grd_rct(col_native, rct(0, 0, 3, -2))
  expect_identical(plot(grid_native_rev_y), grid_native_rev_y)

  grid_native_rev_x <- grd_rct(col_native, rct(0, 0, -3, 2))
  expect_identical(plot(grid_native_rev_x), grid_native_rev_x)
})

test_that("grd_rct() plot method skips plotting when not relevant", {
  grid_numeric <- grd_rct(matrix(0:5, nrow = 2, ncol = 3))

  # so zoomed out that no pixels would be shown
  plot(rct(0, 0, 1e6, 1e6))
  expect_identical(plot(grid_numeric, add = T), grid_numeric)

  # viewport area does not intersect grid
  plot(rct(10, 10, 11, 11))
  expect_identical(plot(grid_numeric, add = T), grid_numeric)
})


test_that("as.raster() works for grd_rct() objects", {
  grid_num <- grd_rct(matrix(1:6, nrow = 2, ncol = 3))
  expect_identical(
    as.raster(grid_num),
    as.raster(matrix(0:5, nrow = 2, ncol = 3) / 5)
  )

  grid_constant <- grd_rct(matrix(0, nrow = 2, ncol = 3))
  expect_identical(
    as.raster(grid_constant),
    as.raster(matrix(0.5, nrow = 2, ncol = 3))
  )

  grid_na <- grd_rct(matrix(NA, nrow = 2, ncol = 3))
  expect_identical(
    as.raster(grid_na),
    as.raster(matrix(NA, nrow = 2, ncol = 3))
  )

  grid_raster <- grd_rct(as.raster(matrix(0:5, nrow = 2, ncol = 3) / 5))
  expect_identical(
    as.raster(grid_num),
    as.raster(matrix(0:5, nrow = 2, ncol = 3) / 5)
  )

  grid_cols <- grd_rct(matrix("#1a1a1a", nrow = 2, ncol = 3))
  expect_identical(
    as.raster(grid_cols),
    as.raster(matrix("#1a1a1a", nrow = 2, ncol = 3))
  )
})

test_that("as.raster() errors for grd_rct() with unsupported type", {
  grid_wut <- grd_rct(array(NA_real_, dim = c(2, 3, 4)))
  expect_error(
    as.raster(grid_wut),
    "Can't convert non-numeric or non-matrix grid to raster image"
  )
})
