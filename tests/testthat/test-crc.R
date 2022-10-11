
test_that("crc class works", {
  expect_s3_class(crc(), "wk_crc")
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

test_that("coercion to and from wk* classes works", {
  expect_s3_class(as_wkt(crc(0, 0, 1)), "wk_wkt")
  expect_s3_class(as_wkb(crc(0, 0, 1)), "wk_wkb")

  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(precision = 2), n_segments = 4),
    wkt("POLYGON ((4 2, 1 5, -2 2, 1 -1, 4 2))")
  )

  expect_identical(
    as_wkb(wk_handle(crc(1, 2, 3), wkt_writer(precision = 2), n_segments = 4)),
    as_wkb("POLYGON ((4 2, 1 5, -2 2, 1 -1, 4 2))")
  )

  # check options for circle resolution + as_wkb/t()
  prev_opt <- options(wk.crc_n_segments = 4)
  expect_length(
    unclass(as_wkb(crc(1, 2, 3)))[[1]],
    1 + 4 + 4 + 4 + 5 * 8 * 2
  )
  options(prev_opt)
})

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

test_that("crc() accessors return the correct components", {
  x <- crc(1, 2, r = 3)
  expect_identical(crc_x(x), 1)
  expect_identical(crc_y(x), 2)
  expect_identical(crc_r(x), 3)
  expect_identical(crc_center(x), xy(1, 2))
})
