
test_that("format() works for wkt", {
  expect_identical(
    wkt_format("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)", max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wkt_format("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)", max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wkt_format(NA_character_), NA_character_)
})

test_that("format() works for wkb", {
  expect_identical(
    wkb_format(wkt_translate_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wkb_format(wkt_translate_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wkb_format(list(NULL)), NA_character_)
})

test_that("format() works for wksxp", {
  expect_identical(
    wksxp_format(wkt_translate_wksxp("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wksxp_format(wkt_translate_wksxp("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wksxp_format(list(NULL)), NA_character_)
})

test_that("format() handles errors", {
  bad_wkb <- wkt_translate_wkb("POINT (30 10)", endian = 1)
  bad_wkb[[1]][2] <- as.raw(0xff)
  expect_match(wkb_format(bad_wkb), "!!!")
  expect_match(wkt_format("POINT ENTPY"), "!!!")
})
