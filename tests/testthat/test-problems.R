
test_that("wk_problems() reports parsing errors for wkb", {
  point <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x24, 0x40))

  expect_identical(wk_problems(new_wk_wkb(list(point))), NA_character_)
  expect_match(wk_problems(new_wk_wkb(list(point[1:5]))), "Unexpected end of buffer")

  point_bad_type <- point
  point_bad_type[3] <- as.raw(0xff)
  expect_match(wk_problems(new_wk_wkb(list(point_bad_type))), "Unrecognized geometry type code")
})

test_that("wk_problems() reports parsing errors for wkt", {
  expect_identical(wk_problems(new_wk_wkt("POINT (30 10)")), NA_character_)
  expect_match(wk_problems(new_wk_wkt("sss")), "Expected geometry type or")
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
    c("Unrecognized geometry type code '255'", rep(NA_character_, length(wkb_good) - 1))
  )
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
