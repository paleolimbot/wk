
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

test_that("wk_plot() errors for geodesic objects", {
  expect_error(wk_plot(wkt(geodesic = TRUE)), "can't plot geodesic objects")
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

test_that("plot can plot all examples", {
  for (which in names(wk_example_wkt)) {
    expect_silent(wk_plot(wk_example_wkt[[!!which]], xlab = which))
  }
})
