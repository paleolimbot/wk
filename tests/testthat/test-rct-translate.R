
test_that("rct_translate_wkt() works", {
  expect_identical(rct_translate_wkt(rct()), character(0))

  expect_identical(
    rct_translate_wkt(rct(1, 2, 3, 4)),
    "POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))"
  )

  expect_identical(
    rct_translate_wkt(rct(NA, NA, NA, NA)),
    "POLYGON EMPTY"
  )

  expect_identical(
    rct_translate_wkt(rct(1, 2, 3, -Inf)),
    "POLYGON EMPTY"
  )

  expect_identical(
    rct_translate_wkt(rct(1, 2, -Inf, 4)),
    "POLYGON EMPTY"
  )

  expect_identical(
    rct_translate_wkt(rct(1, 2, Inf, Inf)),
    "POLYGON ((1 2, inf 2, inf inf, 1 inf, 1 2))"
  )
})

test_that("rct_translate_wkb() works", {
  expect_identical(rct_translate_wkb(rct()), list())
  expect_identical(
    rct_translate_wkb(rct(1, 2, 3, 4)),
    wkt_translate_wkb("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )
})

test_that("rct_translate_wksxp() works", {
  expect_identical(rct_translate_wksxp(rct()), list())
  expect_identical(
    rct_translate_wksxp(rct(1, 2, 3, 4)),
    wkt_translate_wksxp("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )
})
