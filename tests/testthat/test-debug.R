
test_that("debug handlers print messages from the wkt handler", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_output(
    wk_handle(wkt_good, wk_debug_filter()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_error(
    expect_output(
      wk_handle(wkt_bad, wk_debug_filter()),
      "Expected geometry type or 'SRID='"
    ),
    "Expected geometry type or 'SRID='"
  )
})


test_that("debug handlers print messages from the wkb handler", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_output(
    wk_handle(wkb_good, wk_debug_filter()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(
    expect_output(
      wk_handle(new_wk_wkb(wkb_bad), wk_debug_filter()),
      "Unrecognized geometry type code"
    )
  )
})
