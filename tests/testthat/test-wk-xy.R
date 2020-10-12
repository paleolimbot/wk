
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
