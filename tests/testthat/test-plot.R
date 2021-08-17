
test_that("wk_plot() works for zero-length vectors", {
  wk_plot(wkt("POINT (0 1)"))
  expect_identical(wk_plot(wkt(), add = TRUE), wkt())
})

test_that("wk_plot() works for all points", {
  x <- wkt("POINT (0 1)")
  expect_identical(wk_plot(x), x)
})

test_that("wk_plot() works for all point/multipoints", {
  x <- wkt("MULTIPOINT (0 1, 2 2)")
  expect_identical(wk_plot(x), x)
})

test_that("wk_plot() works for all linestrings", {
  x <- wkt("LINESTRING (0 1, 2 2)")
  expect_identical(wk_plot(x), x)
})

test_that("wk_plot() works for all polygons", {
  x <- wkt("POLYGON ((0 0, 0 1, 1 0, 0 0))")
  expect_identical(wk_plot(x), x)
})

test_that("wk_plot() works for all collections", {
  x <- wkt("GEOMETRYCOLLECTION(POLYGON ((0 0, 0 1, 1 0, 0 0)))")
  expect_identical(wk_plot(x), x)
})

test_that("wk_plot() recycles args for each feature", {
  x <- wkt(
    c("GEOMETRYCOLLECTION(POLYGON ((0 0, 0 1, 1 0, 0 0)), POINT (1 0.4))",
      "LINESTRING (0 0, 1 1)"
    )
  )

  expect_identical(wk_plot(x, col = c("blue", "red"), lty = 1), x)

  x <- wkt(c("MULTIPOINT (0 1, 2 2)", "POINT (1 0.4)"))
  expect_identical(wk_plot(x, col = c("blue", "red"), pch = 16), x)
})

test_that("plot methods work", {
  x <- "LINESTRING (0 0, 1 1)"
  expect_identical(plot(as_wkt(x)), as_wkt(x))
  expect_identical(plot(as_wkb(x)), as_wkb(x))
})

test_that("xy and rect plot methods work", {
  expect_identical(plot(xy(1:5, 1:5)), xy(1:5, 1:5))
  expect_identical(plot(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))
})

test_that("crc plot method works", {
  expect_identical(plot(crc(1, 2, 3)), crc(1, 2, 3))
})

test_that("grd_rct() plot method works", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(plot(grid_empty, bbox = rct(0, 0, 1, 1)), grid_empty)

  grid_spec <- grd(nx = 3, ny = 2)
  expect_identical(plot(grid_spec, border = "black"), grid_spec)

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
