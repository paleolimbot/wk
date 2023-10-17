
test_that("xy_writer() works", {
  empties <- wkt(
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  expect_identical(
    wk_handle(empties, xy_writer()),
    rep(xy(NaN, NaN), length(empties))
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

test_that("xy_writer() works with zm dimensions", {
  points_xyzm <- xyzm(1:10, 11:20, 21:30, 31:40)
  expect_identical(
    wk_handle(points_xyzm, xy_writer()),
    points_xyzm
  )

  long_xyzm <- as_wkt(xyzm(runif(2048), runif(2048), runif(2048), runif(2048)))
  expect_identical(
    handle_wkt_without_vector_size(long_xyzm, xy_writer()),
    wk_handle(long_xyzm, xy_writer())
  )
})

test_that("xy_writer() fills unused dimensions with NA", {
  points_xy <- xy(1:10, 11:20)
  points_xyzm <- xyzm(1:10, 11:20, 21:30, 31:40)
  expect_identical(
    wk_handle(c(as_wkb(points_xy), as_wkb(points_xyzm)), xy_writer()),
    c(
      xyzm(1:10, 11:20, NA, NA),
      points_xyzm
    )
  )
})

test_that("xy_writer() can roundtrip point examples", {
  expect_identical(
    wk_handle(wk_example_wkt$point, xy_writer()),
    xy(c(30, NaN, NA), c(10, NaN, NA))
  )

  expect_identical(
    wk_handle(wk_example_wkt$point_z, xy_writer()),
    xyz(c(30, NaN, NA), c(10, NaN, NA), c(40, NaN, NA))
  )

  expect_identical(
    wk_handle(wk_example_wkt$point_m, xy_writer()),
    xym(c(30, NaN, NA), c(10, NaN, NA), c(300, NaN, NA))
  )

  expect_identical(
    wk_handle(wk_example_wkt$point_zm, xy_writer()),
    xyzm(c(30, NaN, NA), c(10, NaN, NA), c(40, NaN, NA), c(300, NaN, NA))
  )
})
