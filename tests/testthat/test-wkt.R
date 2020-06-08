
test_that("wkt class works", {
  x <- wkt("POINT (40 10)")
  expect_is(x, "wk_wkt")
  expect_is(x, "wk_vctr")
  expect_true(is_wk_wkt(x))
  expect_output(print(x), "wk_wkt")
  expect_identical(as.character(x), unclass(x))
  expect_is(wkt(NA), "wk_wkt")

  expect_error(new_wk_wkt(structure(character(), thing = "stuff")), "must be a character")
  expect_error(new_wk_wkt(list()), "must be a character")
  expect_error(wkt("NOPE"), "Encountered 1 parse problem")
  expect_error(wkt(rep("NOPE", 10)), "Encountered 10 parse problems")

  expect_is(x[1], "wk_wkt")
  expect_identical(x[[1]], x[1])
  expect_is(c(x, x), "wk_wkt")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep_len(x, 2), c(x, x))
  expect_length(c(x, x), 2)

  x[1] <- as_wkb("POINT (11 12)")
  expect_identical(x[1], wkt("POINT (11 12)"))
})

test_that("as_wkt() works", {
  x <- wkt("POINT (40 10)")
  expect_identical(as_wkt(x), x)

  # make sure creation options get passed through for identity case
  expect_identical(unclass(x), "POINT (40 10)")
  expect_identical(unclass(as_wkt(x, trim = FALSE, precision = 3)), "POINT (40.000 10.000)")

  expect_identical(as_wkt("POINT (43 44)"), wkt("POINT (43 44)"))
  expect_identical(as_wkt(wkb(wkt_translate_wkb("POINT (99 100)"))), wkt("POINT (99 100)"))
  expect_identical(as_wkt(as_wksxp("POINT (12 13)")), as_wkt("POINT (12 13)"))
})

test_that("parse_wkt() works", {
  x <- "POINT (40 10)"
  parsed <- expect_silent(parse_wkt(x))
  expect_false(is.na(parsed))
  expect_null(attr(parsed, "problems"))

  x <- "POINT ENTPY"
  parsed <- expect_warning(parse_wkt(x), "Encountered 1 parse problem")
  expect_true(is.na(parsed))
  expect_is(attr(parsed, "problems"), "data.frame")
  expect_identical(nrow(attr(parsed, "problems")), 1L)
})
