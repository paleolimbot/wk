
test_that("plot methods work", {
  skip_if_not_installed("wkutils")

  x <- "LINESTRING (0 0, 1 1)"
  expect_identical(plot(as_wkt(x)), as_wkt(x))
  expect_identical(plot(as_wkb(x)), as_wkb(x))
})

test_that("xy and rect plot methods work", {
  expect_identical(plot(xy(1:5, 1:5)), xy(1:5, 1:5))
  expect_identical(plot(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))
})

test_that("crc plot method works", {
  skip_if_not_installed("wkutils")

  expect_identical(plot(crc(1, 2, 3)), crc(1, 2, 3))
})
