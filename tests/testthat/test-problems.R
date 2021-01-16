
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
