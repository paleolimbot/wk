
test_that("xy_writer() works", {
  empties <- wkt(
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  expect_identical(
    wk_handle(empties, xy_writer()),
    rep(xy(NA, NA), length(empties))
  )

  expect_identical(
    wk_handle(wkt("POINT (0 1)"), xy_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("MULTIPOINT ((0 1))"), xy_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("GEOMETRYCOLLECTION (MULTIPOINT ((0 1)))"), xy_writer()),
    xy(0, 1)
  )

  expect_error(
    wk_handle(wkt("LINESTRING (0 1, 1 2)"), xy_writer()),
    "Can't convert geometry"
  )

  expect_error(
    wk_handle(wkt("MULTIPOINT (0 1, 1 2)"), xy_writer()),
    "contains more than one coordinate"
  )
})