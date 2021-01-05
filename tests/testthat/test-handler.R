
test_that("handlers can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
  expect_is(wk_debug_handler(), "wk_debug_handler")
  expect_is(wk_debug_handler(), "wk_handler")
  expect_is(wk_problems_handler(), "wk_problems_handler")
  expect_is(wk_problems_handler(), "wk_handler")
})

test_that("handlers can be run", {
  expect_null(wk_void(wkb()))
  expect_output(expect_null(wk_debug(wkb())))
  expect_identical(wk_problems(wkb()), character())
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

  expect_null(wk_handle(wkb_good, wk_void_handler()))

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wk_void_handler()), "Unrecognized geometry type code")
})

test_that("void handlers cannot be re-used", {
  handler <- wk_void_handler()
  expect_null(wk_handle(as_wkb("POINT (1 1)"), handler))
  expect_error(wk_handle(as_wkb("POINT (1 1)"), handler), "Can't re-use a wk_handler")
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
    wk_handle(wkb_good, wk_debug_handler()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_output(wk_handle(new_wk_wkb(wkb_bad), wk_debug_handler()), "Unrecognized geometry type code")
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
    wk_handle(wkb_good, wk_problems_handler()),
    rep(NA_character_, length(wkb_good))
  )

  wkb_bad <- unclass(wkb_good)
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_identical(
    wk_handle(new_wk_wkb(wkb_bad), wk_problems_handler()),
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
    wk_handle(wkt_good, wk_debug_handler()),
    "POINT.*?LINESTRING.*?POLYGON.*?MULTIPOINT.*?MULTILINESTRING.*?MULTIPOLYGON.*?GEOMETRYCOLLECTION.*?POINT.*?LINESTRING"
  )

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_output(wk_handle(wkt_bad, wk_debug_handler()), "Expected geometry type or 'SRID='")
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

  expect_null(wk_handle(wkt_good, wk_void_handler()))

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_error(wk_handle(wkt_bad, wk_void_handler()), "Expected geometry type or 'SRID='")
})

test_that("void handlers cannot be re-used when called from C++", {
  handler <- wk_void_handler()
  expect_null(wk_handle(as_wkt("POINT (1 1)"), handler))
  expect_error(wk_handle(as_wkt("POINT (1 1)"), handler), "Can't re-use a wk_handler")
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
    wk_handle(wkt_good, wk_problems_handler()),
    rep(NA_character_, length(wkt_good))
  )

  wkt_bad <- unclass(wkt_good)
  wkt_bad[1] <- "NOT WKT"
  expect_identical(
    wk_handle(new_wk_wkt(wkt_bad), wk_problems_handler()),
    c("Expected geometry type or 'SRID=' but found 'NOT' (:1)", rep(NA_character_, length(wkt_good) - 1))
  )
})

test_that("wkt_writer() works", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(wkt_good, wkt_writer()),
    unclass(wkt_good)
  )

  expect_error(wk_handle(new_wk_wkt("NOT WKT"), wkt_writer()), "Expected geometry type or 'SRID")
  expect_identical(
    wk_handle(new_wk_wkt("POINT (1 1)"), wkt_writer(precision = 1, trim = FALSE)),
    "POINT (1.0 1.0)"
  )
})

test_that("wkb_writer() works", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(wkb_good, wkb_writer()),
    unclass(wkb_good)
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wkb_writer()), "Unrecognized geometry type code")
})

test_that("wkb_writer() works with streaming input", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(as_wkt(wkb_good), wkb_writer()),
    unclass(wkb_good)
  )
})

test_that("wkb_writer() errors when the recursion limit is too high", {
  make_really_recursive_geom <- function(n) {
    wkt(paste0(
      c(rep("GEOMETRYCOLLECTION (", n), "POLYGON ((0 1))", rep(")", n)),
      collapse = ""
    ))
  }

  # errors in geometry_start
  expect_error(
    wk_handle(make_really_recursive_geom(31), wkb_writer()),
    "Can't write WKB with maximum"
  )
  # errors in ring_start
  expect_error(
    wk_handle(make_really_recursive_geom(32), wkb_writer()),
    "Can't write WKB with maximum"
  )
})

test_that("wk_handle.wk_xy() works", {
  expect_identical(
    wk_handle(xy(c(NA, 2, 3, NA), c(NA, NA, 4, 5)), wkt_writer()),
    c("POINT EMPTY", "POINT (2 nan)", "POINT (3 4)", "POINT (nan 5)")
  )

  expect_identical(
    wk_handle(xyz(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT Z (2 nan nan)", "POINT Z (3 4 nan)", "POINT Z (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xym(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT M (2 nan nan)", "POINT M (3 4 nan)", "POINT M (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xyzm(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA), c(NA, rep(1, 3))), wkt_writer()),
    c("POINT EMPTY", "POINT ZM (2 nan nan 1)", "POINT ZM (3 4 nan 1)", "POINT ZM (nan 5 nan 1)")
  )
})

test_that("wk_handle.wk_rct() works", {
  expect_identical(
    wk_handle(rct(c(1, NA, Inf, 0), c(2, NA, 0, Inf), c(3, NA, 1, 1), c(4, NA, 1, 1)), wkt_writer()),
    c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", "POLYGON EMPTY", "POLYGON EMPTY", "POLYGON EMPTY")
  )
})
