
test_that("wk_linestring() works", {
  expect_identical(wk_linestring(wkt()), wkt("LINESTRING EMPTY", crs = wk_crs_inherit()))
  expect_identical(
    wk_linestring(wkt(NA_character_)),
    wkt("LINESTRING EMPTY")
  )
  expect_identical(
    wk_linestring(wkt("POINT EMPTY")),
    wkt("LINESTRING EMPTY")
  )

  expect_identical(
    wk_linestring(xy(1:4, 1), feature_id = 1L),
    as_wkb("LINESTRING (1 1, 2 1, 3 1, 4 1)")
  )
  expect_identical(
    wk_linestring(xy(1:4, 1), feature_id = c(1L, 1L, 2L, 2L)),
    as_wkb(c("LINESTRING (1 1, 2 1)", "LINESTRING (3 1, 4 1)"))
  )

  expect_identical(
    wk_linestring(wkt("POLYGON ((0 0, 0 1, 1 0, 0 0))")),
    wkt("LINESTRING (0 0, 0 1, 1 0, 0 0)")
  )

  expect_error(wk_linestring(new_wk_wkt("POINT ENTPY")), "EMPTY")
})

test_that("wk_linestring() errors for inconsistent dimensions/srid", {
  expect_error(
    wk_linestring(wkt(c("POINT (0 1)", "POINT Z (1 2 3)"))),
    "Can't create linestring"
  )
  expect_error(
    wk_linestring(wkt(c("POINT (0 1)", "POINT M (1 2 3)"))),
    "Can't create linestring"
  )
  expect_error(
    wk_linestring(wkt(c("POINT (0 1)", "POINT ZM (1 2 3 4)"))),
    "Can't create linestring"
  )
  expect_error(
    wk_linestring(wkt(c("POINT (0 1)", "SRID=1234;POINT (1 2)"))),
    "Can't create linestring"
  )
})


test_that("wk_collection() works", {
  expect_identical(wk_collection(wkt()), wkt("GEOMETRYCOLLECTION EMPTY", crs = wk_crs_inherit()))
  expect_identical(
    wk_collection(wkt(NA_character_)),
    wkt("GEOMETRYCOLLECTION EMPTY")
  )
  expect_identical(
    wk_collection(wkt("POINT EMPTY")),
    wkt("GEOMETRYCOLLECTION (POINT EMPTY)")
  )
  expect_identical(
    wk_collection(xy(1:4, 1), feature_id = 1L),
    as_wkb("GEOMETRYCOLLECTION (POINT (1 1), POINT (2 1), POINT (3 1), POINT (4 1))")
  )
  expect_identical(
    wk_collection(xy(1:4, 1), feature_id = c(1L, 1L, 2L, 2L)),
    as_wkb(
      c("GEOMETRYCOLLECTION (POINT (1 1), POINT (2 1))",
        "GEOMETRYCOLLECTION (POINT (3 1), POINT (4 1))")
    )
  )

  expect_identical(
    wk_collection(wkt("POLYGON ((0 0, 0 1, 1 0, 0 0))")),
    wkt("GEOMETRYCOLLECTION (POLYGON ((0 0, 0 1, 1 0, 0 0)))")
  )

  expect_error(wk_collection(new_wk_wkt("POINT ENTPY")), "EMPTY")
})
