test_that("wk_orient() works", {
  # ccw to ccw
  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))"),
      direction = wk_counterclockwise()
    ),
    wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))")
  )

  # ccw to cw
  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))"),
      direction = wk_clockwise()
    ),
    wkt("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0), (0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.8, 0.2 0.2))")
  )

  # inconsistent to ccw
  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.8 0.2, 0.8 0.8, 0.2 0.8, 0.2 0.2))"),
      direction = wk_counterclockwise()
    ),
    wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0), (0.2 0.2, 0.2 0.8, 0.8 0.8, 0.8 0.2, 0.2 0.2))")
  )

  # a collection
  expect_identical(
    wk_orient(
      wkt(c("MULTIPOLYGON (((1 1, 2 1, 1 2, 1 1)), ((0 -1, 0 1, 1 0, 0 -1)))")),
      direction = wk_clockwise()
    ),
    wkt(c("MULTIPOLYGON (((1 1, 1 2, 2 1, 1 1)), ((0 -1, 0 1, 1 0, 0 -1)))"))
  )

  # xyz
  expect_identical(
    wk_orient(
      wkt("POLYGON Z ((1 1 5, 2 4 3, 4 3 2, 1 1 5))"),
      direction = wk_clockwise()
    ),
    wkt("POLYGON Z ((1 1 5, 2 4 3, 4 3 2, 1 1 5))")
  )

  # xyzm
  expect_identical(
    wk_orient(
      wkt("POLYGON ZM ((1 1 5 1, 2 4 3 2, 4 3 2 3, 1 1 5 1))"),
      direction = wk_counterclockwise()
    ),
    wkt("POLYGON ZM ((1 1 5 1, 4 3 2 3, 2 4 3 2, 1 1 5 1))")
  )
})

test_that("wk_orient() works for nested collections", {
  expect_identical(
    wk_orient(
      wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (MULTIPOLYGON (((0 0, 1 0, 0 1, 0 0)))))"),
      direction = wk_clockwise()
    ),
    wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)))))")
  )
})

test_that("wk_orient() skips non-polygons", {
  expect_identical(
    wk_orient(
      wkt(c("MULTIPOINT ((0 1))", "LINESTRING (1 1, 2 2, 3 3)", "POLYGON ((0 0, 0 1, 1 0, 0 0))")),
      direction = wk_counterclockwise()
    ),
    wkt(c("MULTIPOINT ((0 1))", "LINESTRING (1 1, 2 2, 3 3)", "POLYGON ((0 0, 1 0, 0 1, 0 0))"))
  )

  expect_identical(
    wk_orient(
      xyzm(1:10, 11:20, -1, -2)
    ),
    xyzm(1:10, 11:20, -1, -2)
  )
})

test_that("wk_orient() skips empty geometries", {
  expect_identical(
    wk_orient(
      wkt(c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY"))
    ),
    wkt(c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY"))
  )

  expect_identical(
    wk_orient(
      wkt(c("MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY", "GEOMETRYCOLLECTION EMPTY"))
    ),
    wkt(c("MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY", "GEOMETRYCOLLECTION EMPTY"))
  )
})

test_that("wk_orient() skips zero-area polygons", {
  # zero area
  expect_identical(
    wk_orient(
      wkt("POLYGON ((1 1, 2 2, 3 3, 1 1))"),
      wk_clockwise()
    ),
    wkt("POLYGON ((1 1, 2 2, 3 3, 1 1))")
  )

  expect_identical(
    wk_orient(
      wkt("POLYGON ((1 1, 2 2, 3 3, 1 1))"),
      wk_counterclockwise()
    ),
    wkt("POLYGON ((1 1, 2 2, 3 3, 1 1))")
  )
})

test_that("wk_orient() handles invalid polygons", {
  # invalid polygon, no coordinates
  expect_identical(
    wk_handle(
      wkt("POLYGON (EMPTY)"),
      # wkt writer will throw here
      wk_orient_filter(
        wkb_writer()
      )
    ),
    wk_handle(
      wkt("POLYGON (EMPTY)"),
      wkb_writer()
    )
  )

  # invalid polygon, no area
  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 1 1))")
    ),
    wkt("POLYGON ((0 0, 1 1))")
  )
})

test_that("wk_orient() preserves attributes", {
  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 0 0.5, 0.5 0.5, 0.5 0, 0 0))", crs = wk_crs_longlat(), geodesic = TRUE),
      direction = wk_clockwise()
    ),
    wkt("POLYGON ((0 0, 0 0.5, 0.5 0.5, 0.5 0, 0 0))", crs = wk_crs_longlat(), geodesic = TRUE)
  )

  expect_identical(
    wk_orient(
      wkt("POLYGON ((0 0, 0 0.5, 0.5 0.5, 0.5 0, 0 0))", crs = wk_crs_longlat(), geodesic = TRUE),
      direction = wk_counterclockwise()
    ),
    wkt("POLYGON ((0 0, 0.5 0, 0.5 0.5, 0 0.5, 0 0))", crs = wk_crs_longlat(), geodesic = TRUE)
  )
})
