
test_that("wk_problems_wkb() reports parsing errors", {
  point <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x24, 0x40))

  expect_identical(wk_problems_wkb(list(point)), NA_character_)
  expect_match(wk_problems_wkb(list(point[1:5])), "Reached end")

  point_bad_type <- point
  point_bad_type[2] <- as.raw(0xff)
  expect_match(wk_problems_wkb(list(point_bad_type)), "Unrecognized geometry type")
})
