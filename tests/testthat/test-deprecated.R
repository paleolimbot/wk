
test_that("format() works for wkt", {
  expect_identical(
    wkt_format("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)", max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wkt_format("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)", max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wkt_format(NA_character_), "<null feature>")
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
  expect_identical(wkb_format(list(NULL)), "<null feature>")
})

test_that("format() handles errors", {
  bad_wkb <- wkt_translate_wkb("POINT (30 10)", endian = 1L)
  bad_wkb[[1]][2] <- as.raw(0xff)
  expect_match(wkb_format(bad_wkb), "!!!")
  expect_match(wkt_format("POINT ENTPY"), "!!!")
})


test_that("wkb_problems() reports parsing errors", {
  point <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x24, 0x40))

  expect_identical(wkb_problems(list(point)), NA_character_)
  expect_match(wkb_problems(list(point[1:5])), "Unexpected end of buffer")

  point_bad_type <- point
  point_bad_type[2] <- as.raw(0xff)
  expect_match(wkb_problems(list(point_bad_type)), "Unrecognized geometry type code")
})

test_that("wkt_problems() reports parsing errors", {
  expect_identical(wkt_problems("POINT (30 10)"), NA_character_)
  expect_match(wkt_problems("sss"), "Expected geometry type or")
})
