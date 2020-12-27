
test_that("handlers can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
  expect_is(wk_debug_handler(), "wk_debug_handler")
  expect_is(wk_debug_handler(), "wk_handler")
  expect_is(wk_validation_handler(), "wk_validation_handler")
  expect_is(wk_validation_handler(), "wk_handler")
})

test_that("wk_handler class works", {
  expect_true(is_wk_handler(wk_void_handler()))
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_output(print(wk_void_handler()), "wk_void_handler")
})

test_that("void handlers do nothing", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_null(handle_wkb(wkb_good, wk_void_handler()))

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(handle_wkb(new_wk_wkb(wkb_bad), wk_void_handler()), "Unrecognized geometry type code")
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
    handle_wkb(wkb_good, wk_debug_handler()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_output(handle_wkb(new_wk_wkb(wkb_bad), wk_debug_handler()), "Unrecognized geometry type code")
})

test_that("validating handlers return a character vector of problems", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    handle_wkb(wkb_good, wk_validation_handler()),
    rep(NA_character_, length(wkb_good))
  )

  wkb_bad <- unclass(wkb_good)
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_identical(
    handle_wkb(new_wk_wkb(wkb_bad), wk_validation_handler()),
    c("Unrecognized geometry type code: 255", rep(NA_character_, length(wkb_good) - 1))
  )
})

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
    handle_wkt(wkt_good, wk_debug_handler()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_output(handle_wkt(wkt_bad, wk_debug_handler()), "Expected geometry type or 'SRID='")
})

test_that("void handlers do nothing when passed to the wkt handler", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_null(handle_wkt(wkt_good, wk_void_handler()))

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_error(handle_wkt(wkt_bad, wk_void_handler()), "Expected geometry type or 'SRID='")
})

test_that("validating handlers return a character vector of problems for WKT", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    handle_wkt(wkt_good, wk_validation_handler()),
    rep(NA_character_, length(wkt_good))
  )

  wkt_bad <- unclass(wkt_good)
  wkt_bad[1] <- "NOT WKT"
  expect_identical(
    handle_wkt(new_wk_wkt(wkt_bad), wk_validation_handler()),
    c("Expected geometry type or 'SRID=' but found 'NOT' (:1)", rep(NA_character_, length(wkt_good) - 1))
  )
})
