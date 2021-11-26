
test_that("wkt class works", {
  x <- wkt("POINT (40 10)")
  expect_s3_class(x, "wk_wkt")
  expect_s3_class(x, "wk_vctr")
  expect_true(is_wk_wkt(x))
  expect_output(print(x), "wk_wkt")
  expect_identical(as.character(x), unclass(x))
  expect_s3_class(wkt(NA), "wk_wkt")

  expect_error(new_wk_wkt(structure(character(), thing = "stuff")), "must be a character")
  expect_error(new_wk_wkt(list()), "must be a character")
  expect_error(wkt("NOPE"), "Encountered 1 parse problem")
  expect_error(wkt(rep("NOPE", 10)), "Encountered 10 parse problems")

  expect_s3_class(x[1], "wk_wkt")
  expect_identical(x[[1]], x[1])
  expect_s3_class(c(x, x), "wk_wkt")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep(wkt(), 1), wkt())
  expect_identical(rep_len(x, 2), c(x, x))
  expect_length(c(x, x), 2)

  x[1] <- as_wkb("POINT (11 12)")
  expect_identical(x[1], wkt("POINT (11 12)"))
})

test_that("wkt() and parse_wkt() strip attributes", {
  text <- structure("POINT (40 10)", some_attr = "value")
  expect_identical(wkt(text), wkt("POINT (40 10)"))
  expect_identical(parse_wkt(text), wkt("POINT (40 10)"))
})

test_that("as_wkt() works", {
  x <- wkt("POINT (40 10)")
  expect_identical(as_wkt(x), x)
  expect_identical(as_wkt("POINT (43 44)"), wkt("POINT (43 44)"))
  expect_identical(as_wkt(wkb(wkt_translate_wkb("POINT (99 100)"))), wkt("POINT (99 100)"))
})

test_that("parse_wkt() works", {
  x <- "POINT (40 10)"
  expect_silent(parsed <- parse_wkt(x))
  expect_false(is.na(parsed))
  expect_null(attr(parsed, "problems"))

  x <- "POINT ENTPY"
  expect_warning(parsed <- parse_wkt(x), "Encountered 1 parse problem")
  expect_true(is.na(parsed))
  expect_s3_class(attr(parsed, "problems"), "data.frame")
  expect_identical(nrow(attr(parsed, "problems")), 1L)
})

test_that("wkt() propagates CRS", {
  x <- wkt("POINT (1 2)")
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wkt(x, crs = NULL), "are not equal")
  x[1] <- wkt(x, crs = 1234L)
  expect_identical(wk_crs(x), 1234)
})
