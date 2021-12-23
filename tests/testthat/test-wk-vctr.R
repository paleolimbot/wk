
test_that("wk_vctr class works", {
  x <- structure(1:5, class = "wk_vctr")
  expect_s3_class(x, "wk_vctr")
  expect_s3_class(x[1:2], "wk_vctr")
  expect_identical(
    c(x, x),
    structure(c(1:5, 1:5), class = "wk_vctr")
  )
  expect_output(print(x), "wk_vctr")
  expect_output(print(stats::setNames(x, as.character(1:5))), "wk_vctr")
  expect_output(print(x[0]), "wk_vctr")
  expect_output(print(wk_set_crs(x, 1234)), "CRS=EPSG:1234")
  expect_output(expect_identical(str(x), x), "wk_vctr")
  expect_output(expect_identical(str(x[0]), x[0]), "wk_vctr\\[0\\]")

  old_opt <- options(max.print = 1000)
  expect_output(
    print(structure(1:1001, class = "wk_vctr")),
    "Reached max.print"
  )
  options(old_opt)

  x[[3]] <- 13L
  expect_identical(unclass(x), c(1L, 2L, 13L, 4L, 5L))

  expect_identical(
    data.frame(col_name = x),
    new_data_frame(list(col_name = x))
  )
  expect_error(as.data.frame(x), "cannot coerce")
})

test_that("geodesic gets printed for geodesic objects", {
  x_geod <- wkt("POINT EMPTY", geodesic = TRUE)
  expect_output(print(x_geod), "geodesic wk_wkt")
})

test_that("rep() works for list wk_vctrs", {
  expect_identical(
    rep(structure(list(NULL), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )

  expect_identical(
    rep(structure(list(), class = "wk_vctr"), 3),
    structure(list(), class = "wk_vctr")
  )
})

test_that("rep() works for chr wk_vctrs", {
  expect_identical(
    rep(structure(NA_character_, class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )

  expect_identical(
    rep(structure(character(), class = "wk_vctr"), 3),
    structure(character(), class = "wk_vctr")
  )
})

test_that("rep_len() works for wk_vctr objects", {
  skip_if_not(packageVersion("base") >= "3.6")

  expect_identical(
    rep_len(structure(list(NULL), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(list(), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(NA_character_, class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(character(), class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )
})


test_that("c() for wk_vctr handles crs attributes", {
  expect_identical(
    wk_crs(c(wkt("POINT (0 1)", crs = wk_crs_inherit()), wkt("POINT (0 2)", crs = 1234))),
    1234
  )

  expect_error(
    wk_crs(c(wkt("POINT (0 1)"), wkt("POINT (0 2)", crs = 1234))),
    "are not equal"
  )
})

test_that("wk_vctr objects with different subclasses can't be combined", {
  expect_error(
    c(as_wkt("POINT EMPTY"), as_wkb("POINT EMPTY")),
    "Can't combine"
  )
})
