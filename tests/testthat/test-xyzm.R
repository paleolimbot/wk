
test_that("wk_xy class works", {
  expect_is(xy(), "wk_xy")
  expect_output(print(xy(1, 2)), "\\(1 2\\)")
  expect_identical(xy_dims(xy()), c("x", "y"))

  expect_identical(as_xy(xy()), xy())
  expect_identical(as_xy(xy(), dims = NULL), xy())
  expect_identical(as_xy(xy(), dims = c("x", "y")), xy())
  expect_identical(as_xy(xy(), dims = c("x", "y", "z")), xyz())
  expect_identical(as_xy(xy(), dims = c("x", "y", "m")), xym())
  expect_identical(as_xy(xy(), dims = c("x", "y", "z", "m")), xyzm())

  expect_identical(as_xy(xy(1, 2), dims = NULL), xy(1, 2))
  expect_identical(as_xy(xy(1, 2), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(xy(1, 2), dims = c("x", "y", "z")), xyz(1, 2, NA))
  expect_identical(as_xy(xy(1, 2), dims = c("x", "y", "m")), xym(1, 2, NA))
  expect_identical(as_xy(xy(1, 2), dims = c("x", "y", "z", "m")), xyzm(1, 2, NA, NA))
})

test_that("wk_xyz class works", {
  expect_is(xyz(), "wk_xyz")
  expect_is(xyz(), "wk_xy")
  expect_output(print(xyz(1, 2, 3)), "Z \\(1 2 3\\)")
  expect_identical(xy_dims(xyz()), c("x", "y", "z"))

  expect_identical(as_xy(xyz()), xyz())
  expect_identical(as_xy(xyz(), dims = NULL), xyz())
  expect_identical(as_xy(xyz(), dims = c("x", "y")), xy())
  expect_identical(as_xy(xyz(), dims = c("x", "y", "z")), xyz())
  expect_identical(as_xy(xyz(), dims = c("x", "y", "m")), xym())
  expect_identical(as_xy(xyz(), dims = c("x", "y", "z", "m")), xyzm())

  expect_identical(as_xy(xyz(1, 2, 3), dims = NULL), xyz(1, 2, 3))
  expect_identical(as_xy(xyz(1, 2, 3), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(xyz(1, 2, 3), dims = c("x", "y", "z")), xyz(1, 2, 3))
  expect_identical(as_xy(xyz(1, 2, 3), dims = c("x", "y", "m")), xym(1, 2, NA))
  expect_identical(as_xy(xyz(1, 2, 3), dims = c("x", "y", "z", "m")), xyzm(1, 2, 3, NA))
})

test_that("wk_xym class works", {
  expect_is(xym(), "wk_xym")
  expect_is(xym(), "wk_xy")
  expect_output(print(xym(1, 2, 3)), "M \\(1 2 3\\)")
  expect_identical(xy_dims(xym()), c("x", "y", "m"))

  expect_identical(as_xy(xym()), xym())
  expect_identical(as_xy(xym(), dims = NULL), xym())
  expect_identical(as_xy(xym(), dims = c("x", "y")), xy())
  expect_identical(as_xy(xym(), dims = c("x", "y", "z")), xyz())
  expect_identical(as_xy(xym(), dims = c("x", "y", "m")), xym())
  expect_identical(as_xy(xym(), dims = c("x", "y", "z", "m")), xyzm())

  expect_identical(as_xy(xym(1, 2, 3), dims = NULL), xym(1, 2, 3))
  expect_identical(as_xy(xym(1, 2, 3), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(xym(1, 2, 3), dims = c("x", "y", "z")), xyz(1, 2, NA))
  expect_identical(as_xy(xym(1, 2, 3), dims = c("x", "y", "m")), xym(1, 2, 3))
  expect_identical(as_xy(xym(1, 2, 3), dims = c("x", "y", "z", "m")), xyzm(1, 2, NA, 3))
})

test_that("wk_xyzm class works", {
  expect_is(xyzm(), "wk_xyzm")
  expect_is(xyzm(), "wk_xyz")
  expect_is(xyzm(), "wk_xym")
  expect_is(xyzm(), "wk_xy")
  expect_output(print(xyzm(1, 2, 3, 4)), "ZM \\(1 2 3 4\\)")
  expect_identical(xy_dims(xyzm()), c("x", "y", "z", "m"))

  expect_identical(as_xy(xyzm()), xyzm())
  expect_identical(as_xy(xyzm(), dims = NULL), xyzm())
  expect_identical(as_xy(xyzm(), dims = c("x", "y")), xy())
  expect_identical(as_xy(xyzm(), dims = c("x", "y", "z")), xyz())
  expect_identical(as_xy(xyzm(), dims = c("x", "y", "m")), xym())
  expect_identical(as_xy(xyzm(), dims = c("x", "y", "z", "m")), xyzm())

  expect_identical(as_xy(xyzm(1, 2, 3, 4), dims = NULL), xyzm(1, 2, 3, 4))
  expect_identical(as_xy(xyzm(1, 2, 3, 4), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(xyzm(1, 2, 3, 4), dims = c("x", "y", "z")), xyz(1, 2, 3))
  expect_identical(as_xy(xyzm(1, 2, 3, 4), dims = c("x", "y", "m")), xym(1, 2, 4))
  expect_identical(as_xy(xyzm(1, 2, 3, 4), dims = c("x", "y", "z", "m")), xyzm(1, 2, 3, 4))
})

test_that("wk_xy* are vctrs", {
  expect_true(vctrs::vec_is(xy()))
  expect_true(vctrs::vec_is(xyz()))
  expect_true(vctrs::vec_is(xym()))
  expect_true(vctrs::vec_is(xyzm()))
})

test_that("wk_xy* vectors can be constructed from matrices/data.frames", {
  expect_identical(as_xy(data.frame(x = 1, y = 2, z = 3, m = 4), dims = NULL), xyzm(1, 2, 3, 4))
  expect_identical(as_xy(data.frame(x = 1, y = 2, z = 3, m = 4), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(data.frame(x = 1, y = 2, z = 3, m = 4), dims = c("x", "y", "z")), xyz(1, 2, 3))
  expect_identical(as_xy(data.frame(x = 1, y = 2, z = 3, m = 4), dims = c("x", "y", "m")), xym(1, 2, 4))
  expect_identical(as_xy(data.frame(x = 1, y = 2, z = 3, m = 4), dims = c("x", "y", "z", "m")), xyzm(1, 2, 3, 4))

  expect_identical(as_xy(data.frame(x = 1, y = 2), dims = NULL), xy(1, 2))
  expect_identical(as_xy(data.frame(x = 1, y = 2), dims = c("x", "y")), xy(1, 2))
  expect_identical(as_xy(data.frame(x = 1, y = 2), dims = c("x", "y", "z")), xyz(1, 2, NA))
  expect_identical(as_xy(data.frame(x = 1, y = 2), dims = c("x", "y", "m")), xym(1, 2, NA))
  expect_identical(as_xy(data.frame(x = 1, y = 2), dims = c("x", "y", "z", "m")), xyzm(1, 2, NA, NA))

  expect_error(as_xy(data.frame(x = 1, y = 2), dims = "L"), "Unknown dims")

  expect_identical(
    as_xy(as.matrix(data.frame(x = 1, y = 2, z = 3, m = 4))),
    xyzm(1, 2, 3, 4)
  )
  expect_identical(
    as_xy(matrix(1:2, nrow = 1)),
    xy(1, 2)
  )
  expect_identical(
    as_xy(matrix(1:3, nrow = 1)),
    xyz(1, 2, 3)
  )
  expect_identical(
    as_xy(matrix(1:4, nrow = 1)),
    xyzm(1, 2, 3, 4)
  )

  expect_identical(
    as_xy(matrix(1:2, nrow = 1, dimnames = list(NULL, c("x", "y")))),
    xy(1, 2)
  )
  expect_identical(
    as_xy(matrix(1:3, nrow = 1, dimnames = list(NULL, c("x", "y", "m")))),
    xym(1, 2, 3)
  )

  expect_error(as_xy(matrix(1:10, nrow = 1)), "Can't guess dimensions")
})

test_that("coercion to wk* vectors works", {
  expect_identical(as_wkt(xy(1, 2)), wkt("POINT (1 2)"))
  expect_identical(as_wkb(xy(1, 2)), as_wkb("POINT (1 2)"))
})

test_that("coercion from wk* vectors works", {
  expect_identical(as_xy(wkt("POINT (1 2)")), xy(1, 2))
  expect_identical(as_xy(wkt("POINT Z (1 2 3)")), xyz(1, 2, 3))
  expect_identical(as_xy(wkt("POINT M (1 2 4)")), xym(1, 2, 4))
  expect_identical(as_xy(wkt("POINT ZM (1 2 3 4)")), xyzm(1, 2, 3, 4))
  expect_identical(as_xy(wkt("POINT (1 2)"), dims = c("x", "y", "z", "m")), xyzm(1, 2, NA, NA))

  expect_identical(as_xy(as_wkb("POINT (1 2)")), xy(1, 2))

  expect_error(as_xy(wkt("POINT (1 2)"), dims = "L"), "Unknown dims")
})

test_that("subset-assign works for wk_xy", {
  x <- xyzm(1:2, 2, 3, 4)
  x[2] <- xy(10, 20)
  expect_identical(x[2], xyzm(10, 20, NA, NA))
})

test_that("xy() propagates CRS", {
  x <- xy(1, 2)
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wk_set_crs(x, NULL), "are not equal")
  x[1] <- wk_set_crs(x, 1234L)
  expect_identical(wk_crs(x), 1234)
})
