
test_that("the wk_writer() generic resolves correct handler", {
  expect_is(wk_writer(wkt()), "wk_wkt_writer")
  expect_is(wk_writer(wkb()), "wk_wkb_writer")
  expect_is(wk_writer(xy()), "wk_xyzm_writer")
  expect_is(wk_writer(structure(list(), class = "sfc")), "wk_sfc_writer")
})

test_that("xyzm_writer() works", {
  empties <- wkt(
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  expect_identical(
    wk_handle(empties, xyzm_writer()),
    rep(xy(NA, NA), length(empties))
  )

  expect_identical(
    wk_handle(wkt("POINT (0 1)"), xyzm_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("MULTIPOINT ((0 1))"), xyzm_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("GEOMETRYCOLLECTION (MULTIPOINT ((0 1)))"), xyzm_writer()),
    xy(0, 1)
  )

  expect_error(
    wk_handle(wkt("LINESTRING (0 1, 1 2)"), xyzm_writer()),
    "Can't convert geometry"
  )

  expect_error(
    wk_handle(wkt("MULTIPOINT (0 1, 1 2)"), xyzm_writer()),
    "contains more than one coordinate"
  )
})
