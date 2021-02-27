
test_that("wk_vctr class works", {
  x <- structure(1:5, class = "wk_vctr")
  expect_s3_class(x, "wk_vctr")
  expect_s3_class(x[1:2], "wk_vctr")
  expect_output(print(x), "wk_vctr")
  expect_output(print(stats::setNames(x, as.character(1:5))), "wk_vctr")
  expect_output(print(x[0]), "wk_vctr")
  expect_output(print(wk_set_crs(x, 1234)), "CRS=1234")
  expect_output(expect_identical(str(x), x), "wk_vctr")
  expect_output(expect_identical(str(x[0]), x[0]), "wk_vctr\\[0\\]")

  expect_identical(
    data.frame(col_name = x),
    new_data_frame(list(col_name = x))
  )
  expect_error(as.data.frame(x), "cannot coerce")
})

test_that("rep() and rep_len() works for list wk_vctrs", {
  expect_identical(
    rep(structure(list(NULL), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(list(NULL), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )

  expect_identical(
    rep(structure(list(), class = "wk_vctr"), 3),
    structure(list(), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(list(), class = "wk_vctr"), 3),
    structure(list(NULL, NULL, NULL), class = "wk_vctr")
  )
})

test_that("rep() and rep_len() works for chr wk_vctrs", {
  expect_identical(
    rep(structure(NA_character_, class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(NA_character_, class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )

  expect_identical(
    rep(structure(character(), class = "wk_vctr"), 3),
    structure(character(), class = "wk_vctr")
  )

  expect_identical(
    rep_len(structure(character(), class = "wk_vctr"), 3),
    structure(rep(NA_character_, 3), class = "wk_vctr")
  )
})

test_that("wk_vctr objects with different subclasses can't be combined", {
  expect_error(
    c(as_wkt("POINT EMPTY"), as_wkb("POINT EMPTY")),
    "Can't combine"
  )
})
