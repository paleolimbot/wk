
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
    wkt_translate_wkt("MULTIPOINT (30 10, 0 0, 10 10)"),
    "MULTIPOINT ((30 10), (0 0), (10 10))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT ((30 10), (0 0), (10 10))"),
    "MULTIPOINT ((30 10), (0 0), (10 10))"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))"),
    "MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"),
    "MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"
  )
  expect_identical(
    wkt_translate_wkt(
      "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
    ),
    "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
  )
})

test_that("basic translation works on empty geoms", {
  expect_identical(
    wkt_translate_wkt("POINT EMPTY"),
    "POINT EMPTY"
  )
  expect_identical(
    wkt_translate_wkt("LINESTRING EMPTY"),
    "LINESTRING EMPTY"
  )
  expect_identical(
    wkt_translate_wkt("POLYGON EMPTY"),
    "POLYGON EMPTY"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT EMPTY"),
    "MULTIPOINT EMPTY"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING EMPTY"),
    "MULTILINESTRING EMPTY"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON EMPTY"),
    "MULTIPOLYGON EMPTY"
  )
  expect_identical(
    wkt_translate_wkt(
      "GEOMETRYCOLLECTION EMPTY"
    ),
    "GEOMETRYCOLLECTION EMPTY"
  )
})

test_that("mutli* geometries can contain empties", {
  expect_identical(
    wkt_translate_wkt("MULTIPOINT (EMPTY)"),
    "MULTIPOINT (EMPTY)"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT ((1 1), EMPTY)"),
    "MULTIPOINT ((1 1), EMPTY)"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING (EMPTY)"),
    "MULTILINESTRING (EMPTY)"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING ((1 1, 2 4), EMPTY)"),
    "MULTILINESTRING ((1 1, 2 4), EMPTY)"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON (((1 1, 2 4, 3 6)), EMPTY)"),
    "MULTIPOLYGON (((1 1, 2 4, 3 6)), EMPTY)"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON (EMPTY)"),
    "MULTIPOLYGON (EMPTY)"
  )
})

test_that("Z, ZM, and M prefixes are parsed", {
  expect_identical(
    wkt_translate_wkt("POINT (30 10)"),
    "POINT (30 10)"
  )
  expect_identical(
    wkt_translate_wkt("POINT Z (30 10 1)"),
    "POINT Z (30 10 1)"
  )
  expect_identical(
    wkt_translate_wkt("POINT M (30 10 1)"),
    "POINT M (30 10 1)"
  )
  expect_identical(
    wkt_translate_wkt("POINT ZM (30 10 0 1)"),
    "POINT ZM (30 10 0 1)"
  )
})

test_that("SRID prefixes are parsed", {
  expect_identical(
    wkt_translate_wkt("SRID=218;POINT (30 10)"),
    "SRID=218;POINT (30 10)"
  )
})

test_that("correctly formatted ZM geomteries are translated identically", {
  expect_identical(
    wkt_translate_wkt("POINT ZM (30 10 0 1)"),
    "POINT ZM (30 10 0 1)"
  )
  expect_identical(
    wkt_translate_wkt("LINESTRING ZM (30 10 0 1, 0 0 2 3)"),
    "LINESTRING ZM (30 10 0 1, 0 0 2 3)"
  )
  expect_identical(
    wkt_translate_wkt("POLYGON ZM ((30 10 2 1, 0 0 9 10, 10 10 10 8, 30 10 3 8))"),
    "POLYGON ZM ((30 10 2 1, 0 0 9 10, 10 10 10 8, 30 10 3 8))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT ZM (30 10 32 1, 0 0 2 8, 10 10 1 99)"),
    "MULTIPOINT ZM ((30 10 32 1), (0 0 2 8), (10 10 1 99))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOINT ZM ((30 10 32 1), (0 0 2 8), (10 10 1 99))"),
    "MULTIPOINT ZM ((30 10 32 1), (0 0 2 8), (10 10 1 99))"
  )
  expect_identical(
    wkt_translate_wkt("MULTILINESTRING ZM ((30 10 2 1, 0 0 2 8), (20 20 1 1, 0 0 2 2))"),
    "MULTILINESTRING ZM ((30 10 2 1, 0 0 2 8), (20 20 1 1, 0 0 2 2))"
  )
  expect_identical(
    wkt_translate_wkt("MULTIPOLYGON ZM (((30 10 1 3, 0 0 9 1, 10 10 5 9, 30 10 1 2)))"),
    "MULTIPOLYGON ZM (((30 10 1 3, 0 0 9 1, 10 10 5 9, 30 10 1 2)))"
  )
  expect_identical(
    wkt_translate_wkt("GEOMETRYCOLLECTION (POINT ZM (30 10 1 2))"),
    "GEOMETRYCOLLECTION (POINT ZM (30 10 1 2))"
  )
})
