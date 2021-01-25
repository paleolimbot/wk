
test_that("void handler can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
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
  expect_error(wk_handle(as_wkb("POINT (1 1)"), handler), "Can't re-use this wk_handler")
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
  expect_error(wk_handle(as_wkt("POINT (1 1)"), handler), "Can't re-use this wk_handler")
})
