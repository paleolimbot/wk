
test_that("rct class works", {
  expect_is(rct(), "wk_rct")
  expect_output(print(rct(1, 2, 3, 4)), "\\[1 2 3 4\\]")
  expect_identical(as_rct(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))

  expect_identical(
    as_rct(as.matrix(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4))),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4)),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(matrix(1:4, nrow = 1)),
    rct(1, 2, 3, 4)
  )
})

test_that("coercion to and from wk* classes works", {
  expect_identical(
    as_wkt(rct(1, 2, 3, 4)),
    wkt("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )

  expect_identical(
    as_wkb(rct(1, 2, 3, 4)),
    as_wkb("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )

  expect_identical(
    as_wksxp(rct(1, 2, 3, 4)),
    as_wksxp("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )
})

test_that("subset-assign works for rct", {
  x <- rct(1:2, 2:3, 3:4, 4:5)
  x[1] <- rct(NA, NA, NA, NA)
  expect_identical(x, c(rct(NA, NA, NA, NA), rct(2, 3, 4, 5)))
})

test_that("rct() propagates CRS", {
  x <- rct(1, 2, 3, 4)
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wk_set_crs(x, NULL), "are not equal")
  x[1] <- wk_set_crs(x, 1234L)
  expect_identical(wk_crs(x), 1234)
})
