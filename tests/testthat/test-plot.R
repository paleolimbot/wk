
test_that("plot methods work", {
  x <- "LINESTRING (0 0, 1 1)"
  expect_identical(plot(as_wkt(x)), as_wkt(x))
  expect_identical(plot(as_wkb(x)), as_wkb(x))
  expect_identical(plot(as_wksxp(x)), as_wksxp(x))
})
