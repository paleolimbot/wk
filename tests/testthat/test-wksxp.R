
test_that("wksxp class works", {
  x <- wksxp(wkt_translate_wksxp("POINT (40 10)"))
  expect_is(x, "wk_wksxp")
  expect_is(x, "wk_vctr")
  expect_true(is_wk_wksxp(x))
  expect_output(print(x), "wk_wksxp")
  expect_match(as.character(x), "POINT")

  expect_is(wksxp(list(NULL)), "wk_wksxp")
  expect_true(is.na(wksxp(list(NULL))))

  expect_error(new_wk_wksxp(structure(list(), thing = "stuff")), "must be a list")
  expect_error(new_wk_wksxp("char!"), "must be a list")
  expect_error(wksxp(list(raw())), "Encountered 1 parse problem")
  expect_error(wksxp(rep(list(raw()), 10)), "Encountered 10 parse problem")

  expect_is(x[1], "wk_wksxp")
  expect_identical(x[[1]], x[1])
  expect_is(c(x, x), "wk_wksxp")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep_len(x, 2), c(x, x))
  expect_identical(rep(wksxp(), 5), wksxp())
  expect_length(c(x, x), 2)

  x[1] <- "POINT (11 12)"
  expect_identical(x[1], as_wksxp("POINT (11 12)"))
})

test_that("as_wksxp() works", {
  x <- wksxp(wkt_translate_wksxp("SRID=44;POINT (40 10)"))
  expect_identical(as_wksxp(x), x)

  # make sure creation options get passed through for identity case
  expect_identical(attr(unclass(as_wksxp(x, include_srid = TRUE))[[1]], "srid"), 44)
  expect_identical(attr(unclass(as_wksxp(x, include_srid = FALSE))[[1]], "srid"), NULL)

  expect_identical(as_wksxp("SRID=44;POINT (40 10)"), x)
  expect_identical(as_wksxp(wkt("SRID=44;POINT (40 10)")), x)
  expect_identical(as_wksxp(as_wkb("SRID=44;POINT (40 10)")), x)

  # default method
  expect_identical(as_wksxp.default("POINT (11 12)"), as_wksxp("POINT (11 12)"))

  # blob and WKB methods
  expect_identical(
    as_wksxp(structure(wkt_translate_wkb("POINT (11 12)"), class = "blob")),
    as_wksxp("POINT (11 12)")
  )
  expect_identical(
    as_wksxp(structure(wkt_translate_wkb("POINT (11 12)"), class = "WKB")),
    as_wksxp("POINT (11 12)")
  )
})

test_that("wkt() propagates CRS", {
  x <- as_wksxp("POINT (1 2)")
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wksxp(x, crs = NULL), "are not equal")
  x[1] <- wksxp(x, crs = 1234L)
  expect_identical(wk_crs(x), 1234)
})
