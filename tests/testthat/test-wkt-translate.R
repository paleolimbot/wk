
test_that("basic translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wkt("POINT (30 10)"),
    "POINT (30 10)"
  )
  expect_identical(
    wkt_translate_wkt("LINESTRING (30 10, 0 0)"),
    "LINESTRING (30 10, 0 0)"
  )
  expect_identical(
    wkt_translate_wkt("POLYGON ((30 10, 0 0, 10 10, 30 10))"),
    "POLYGON ((30 10, 0 0, 10 10, 30 10))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT ((30 10), (0 0), (10 10))"),
    "MULTIPOINT ((30 10), (0 0), (10 10))"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING ((30 10, 0 0))"),
    "MULTILINESTRING ((30 10, 0 0))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)))"),
    "MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)))"
  )
  expect_identical(
    wkt_translate_wkt(
      "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
    ),
    "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
  )
})
