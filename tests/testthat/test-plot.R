
test_that("plot methods work", {
  skip_if_not_installed("wkutils")

  x <- "LINESTRING (0 0, 1 1)"
  expect_identical(plot(as_wkt(x)), as_wkt(x))
  expect_identical(plot(as_wkb(x)), as_wkb(x))
  expect_identical(plot(as_wksxp(x)), as_wksxp(x))
})

test_that("xy and rect plot methods work", {
  expect_identical(plot(xy(1:5, 1:5)), xy(1:5, 1:5))
  expect_identical(plot(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))
})
