
test_that("crc class works", {
  expect_is(crc(), "wk_crc")
  expect_output(print(crc(1, 2, 3)), "\\[1 2, r = 3\\]")
  expect_identical(as_crc(crc(1, 2, 3)), crc(1, 2, 3))

  expect_identical(
    as_crc(as.matrix(data.frame(x = 1, y = 2, r = 3))),
    crc(1, 2, 3)
  )
  expect_identical(
    as_crc(data.frame(x = 1, y = 2, r = 3)),
    crc(1, 2, 3)
  )
  expect_identical(
    as_crc(matrix(1:3, nrow = 1)),
    crc(1, 2, 3)
  )
})

# test_that("coercion to and from wk* classes works", {
#   expect_identical(
#     as_wkt(crc(1, 2, 3, 4)),
#     wkt("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
#   )
#
#   expect_identical(
#     as_wkb(crc(1, 2, 3, 4)),
#     as_wkb("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
#   )
# })

test_that("subset-assign works for crc", {
  x <- crc(1:2, 2:3, 3:4)
  x[1] <- crc(NA, NA, NA)
  expect_identical(x, c(crc(NA, NA, NA), crc(2, 3, 4)))
})

test_that("crc() propagates CRS", {
  x <- crc(1, 2, 3)
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wk_set_crs(x, NULL), "are not equal")
  x[1] <- wk_set_crs(x, 1234L)
  expect_identical(wk_crs(x), 1234)
})
