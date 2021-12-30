
test_that("format() works for wkt", {
  expect_identical(
    wk_format(wkt("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wk_format(wkt("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wk_format(wkt(NA_character_)), "<null feature>")
})

test_that("format() works for wkb", {
  expect_identical(
    wk_format(as_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wk_format(as_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wk_format(wkb(list(NULL))), "<null feature>")
})

test_that("format() handles errors", {
  bad_wkb <- unclass(wk_handle(wkt("POINT (30 10)"), wkb_writer(endian = 1L)))
  bad_wkb[[1]][2:3] <- as.raw(0xff)
  expect_match(wk_format(new_wk_wkb(bad_wkb)), "!!!")
  expect_match(wk_format(new_wk_wkt("POINT ENTPY")), "!!!")
})


test_that("wkb_problems() reports parsing errors", {
  point <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x24, 0x40))

  expect_identical(wk_problems(wkb(list(point))), NA_character_)
  expect_match(wk_problems(new_wk_wkb(list(point[1:5]))), "Unexpected end of buffer")

  point_bad_type <- point
  point_bad_type[2:3] <- as.raw(0xff)
  expect_match(wk_problems(new_wk_wkb(list(point_bad_type))), "Unrecognized geometry type code")
})

test_that("wkt_problems() reports parsing errors", {
  expect_identical(wk_problems(new_wk_wkt("POINT (30 10)")), NA_character_)
  expect_match(wk_problems(new_wk_wkt("sss")), "Expected geometry type or")
})
