
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

test_that("wk_polygon() works", {
  expect_identical(wk_polygon(xy(double(), double())), as_wkb("POLYGON EMPTY", crs = wk_crs_inherit()))
  expect_identical(
    wk_polygon(xy(c(0, 10, 0), c(0, 0, 10))),
    as_wkb("POLYGON ((0 0, 10 0, 0 10, 0 0))")
  )
  expect_identical(
    wk_polygon(xy(c(0, 10, 0, 0), c(0, 0, 10, 0))),
    as_wkb("POLYGON ((0 0, 10 0, 0 10, 0 0))")
  )

  expect_identical(
    wk_polygon(
      xy(
        c(20, 10, 10, 30, 45, 30, 20, 20),
        c(35, 30, 10, 5, 20, 20, 15, 25)
      ),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2)
    ),
    as_wkb("POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))")
  )

  expect_identical(
    wk_polygon(
      xy(
        c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
        c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30)
      ),
      feature_id = c(rep(1, 8), rep(2, 3)),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    ),
    as_wkb(
      c(
        "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
        "POLYGON ((40 40, 20 45, 45 30, 40 40))"
      )
    )
  )

  expect_identical(
    wk_polygon(
      xy(
        c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
        c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30)
      ),
      feature_id = c(rep(1, 8), rep(2, 3)),
      # new ring should be detected on new feature_id
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
    ),
    as_wkb(
      c(
        "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
        "POLYGON ((40 40, 20 45, 45 30, 40 40))"
      )
    )
  )
})

test_that("wk_polygon() can use a POLYGON input", {
  expect_identical(
    wk_polygon(wkt("POLYGON ((40 40, 20 45, 45 30, 40 40))")),
    wkt("POLYGON ((40 40, 20 45, 45 30, 40 40))")
  )
})

test_that("wk_polygon passes on errors", {
  expect_error(wk_polygon(new_wk_wkt("POLYGON ENTPY")), "ENTPY")
})

test_that("wk_polygon() treats NA as empty", {
  expect_identical(
    wk_polygon(wkt(c("POLYGON ((40 40, 20 45, 45 30, 40 40))", NA))),
    wkt("POLYGON ((40 40, 20 45, 45 30, 40 40))")
  )
})

test_that("wk_polygon() requires consistent dimensions within a feature", {
  expect_error(
    wk_polygon(wkt(c("POINT (0 1)", "POINT Z (1 2 3)"))),
    "Can't create polygon"
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
