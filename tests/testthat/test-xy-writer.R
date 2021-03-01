
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

test_that("xy_writer() works for a vector of indeterminate length", {
  long_xy <- as_wkt(xy(runif(2048), runif(2048)))
  expect_identical(
    handle_wkt_without_vector_size(long_xy, xy_writer()),
    wk_handle(long_xy, xy_writer())
  )
})
