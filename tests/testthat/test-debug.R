
test_that("debug handlers print messages from the wkt handler", {
  wkt_good <- as_wkt(
    c(
      NA, "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_output(
    wk_handle(wkt_good, wk_debug_filter()),
    "null_feature.*?POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
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

test_that("vector attributes are printed by wk_debug()", {
  skip_if_not_installed("sf")

  # sfc is currently the only handler that has vector types, dims, and WK_ABORT
  expect_output(wk_debug(sf::st_sfc(sf::st_point())), "POINT B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_linestring())), "LINESTRING B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_polygon())), "POLYGON B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_multipoint())), "MULTIPOINT B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_multilinestring())), "MULTILINESTRING B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_multipolygon())), "MULTIPOLYGON B\\[1\\]")
  expect_output(wk_debug(sf::st_sfc(sf::st_geometrycollection())), "GEOMETRYCOLLECTION B\\[1\\]")

  expect_output(wk_debug(sf::st_sfc(sf::st_point(c(1, 2, 3, 4)))), "POINT ZMB.*?POINT ZM")
  expect_output(wk_debug(sf::st_sfc()), "\\[EMPTY\\]")

  expect_output(wk_debug(sf::st_as_sfc("POINT (1 2)"), wk_bbox_handler()), "WK_ABORT")
})

test_that("wk_debug() prints error information", {
  expect_output(
    wk_debug(new_wk_wkt("NOT WKT"), wk_problems_handler()),
    "=> WK_ABORT_FEATURE"
  )
})

test_that("wk_debug() runs the debug handler", {
  expect_identical(
    expect_output(
      wk_debug(wkt("POINT (1 2)"), handler = wkb_writer()),
      "POINT"
    ),
    as_wkb("POINT (1 2)")
  )
})
