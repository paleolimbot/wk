
test_that("void and debug handlers can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
  expect_is(wk_debug_handler(), "wk_debug_handler")
  expect_is(wk_debug_handler(), "wk_handler")
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

  handle_wkb(wkb_good, wk_void_handler())

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(handle_wkb(new_wk_wkb(wkb_bad), wk_void_handler()), "Unrecognized geometry type code")
})

test_that("debug handlers print messages", {
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
