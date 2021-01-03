
test_that("xyzm_translate_wkt() works", {
  expect_identical(xyzm_translate_wkt(xyzm()), character(0))
  expect_identical(xyzm_translate_wkt(xyzm(NA, NA, NA, NA)), "POINT EMPTY")
  expect_identical(xyzm_translate_wkt(xyzm(1, 2, 3, 4)), "POINT ZM (1 2 3 4)")
  expect_identical(xyzm_translate_wkt(xyzm(1, 2, NA, NA)), "POINT (1 2)")
  expect_identical(xyzm_translate_wkt(xyzm(1, 2, 3, NA)), "POINT Z (1 2 3)")
  expect_identical(xyzm_translate_wkt(xyzm(1, 2, NA, 4)), "POINT M (1 2 4)")
})

test_that("wkt_translate_xyzm() works", {
  expect_identical(wkt_translate_xyzm(character()), unclass(xy(crs = NULL)))
  expect_identical(wkt_translate_xyzm(character(), include_z = TRUE), unclass(xyz(crs = NULL)))
  expect_identical(wkt_translate_xyzm(character(), include_m = TRUE), unclass(xym(crs = NULL)))
  expect_identical(wkt_translate_xyzm(character(), include_z = TRUE, include_m = TRUE), unclass(xyzm(crs = NULL)))

  expect_identical(
    wkt_translate_xyzm("POINT ZM (1 2 3 4)"),
    unclass(xyzm(1, 2, 3, 4))
  )
  expect_identical(
    wkt_translate_xyzm("POINT Z (1 2 3)"),
    unclass(xyz(1, 2, 3))
  )
  expect_identical(
    wkt_translate_xyzm("POINT M (1 2 4)"),
    unclass(xym(1, 2, 4))
  )
  expect_identical(
    wkt_translate_xyzm("POINT (1 2)"),
    unclass(xy(1, 2))
  )

  expect_identical(wkt_translate_xyzm(NA_character_), unclass(xy(NaN, NaN)))
  expect_identical(wkt_translate_xyzm("POINT EMPTY"), unclass(xy(NaN, NaN)))
  expect_error(wkt_translate_xyzm("LINESTRING (0 0, 1 1)"), "Can't create xy", class = "error")
})

test_that("xyzm_translate_wkb() works", {
  expect_identical(xyzm_translate_wkb(xyzm(crs = NULL)), list())
  expect_identical(xyzm_translate_wkb(xyzm(1, 2, 3, 4)), wkt_translate_wkb("POINT ZM (1 2 3 4)"))
})

test_that("wkb_translate_xyzm() works", {
  expect_identical(wkb_translate_xyzm(list()), unclass(xy(crs = NULL)))
  expect_identical(wkb_translate_xyzm(wkt_translate_wkb("POINT ZM (1 2 3 4)")), unclass(xyzm(1, 2, 3, 4)))
})
