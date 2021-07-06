
test_that("wk_trans_affine() works", {
  expect_s3_class(wk_affine_identity(), "wk_trans_affine")
  expect_output(print(wk_affine_identity()), "wk_trans_affine")
})

test_that("wk_trans_affine() errors for invalid matrix", {
  expect_error(wk_trans_affine(5), "must be a 3x3 matrix")
})

test_that("wk_affine_translate() works", {
  coords <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3), nrow = 2)
  expect_equal(
    as.matrix(wk_affine_translate(2, 3)) %*% rbind(coords, 1),
    rbind(matrix(c(2, 3, 3, 4, 4, 5, 5, 6), nrow = 2), 1)
  )
})

test_that("wk_affine_rotate() works", {
  coords <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3), nrow = 2)
  expect_equal(
    as.matrix(wk_affine_rotate(45)) %*% rbind(coords, 1),
    rbind(matrix(c(0, 0, 0, sqrt(2), 0, 2 * sqrt(2), 0, 3 * sqrt(2)), nrow = 2), 1)
  )
})

test_that("wk_affine_scale() works", {
  coords <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3), nrow = 2)
  expect_equal(
    as.matrix(wk_affine_scale(2, 3)) %*% rbind(coords, 1),
    rbind(matrix(c(0, 0, 2, 3, 4, 6, 6, 9), nrow = 2), 1)
  )
})

test_that("wk_affine_fit() works", {
  src <- xy(c(0, 1, 0), c(0, 0, 1))
  dst <- xy(c(0, 2, 0), c(0, 0, 3))

  expect_equal(
    as.matrix(wk_affine_fit(src, dst)),
    as.matrix(wk_affine_scale(2, 3))
  )

  expect_equal(
    wk_transform(src, wk_affine_fit(src, dst)),
    dst
  )
})

test_that("wk_affine_compose() works", {
  expect_identical(
    as.matrix(wk_affine_compose()),
    as.matrix(wk_affine_identity())
  )

  comp <- wk_affine_compose(
    wk_affine_translate(1, 2),
    wk_affine_translate(3, 4)
  )

  expect_s3_class(comp, "wk_trans_affine")
  expect_identical(as.matrix(comp), as.matrix(wk_affine_translate(4, 6)))

  comp <- wk_affine_compose(
    wk_affine_rotate(12),
    wk_affine_rotate(13)
  )
  expect_equal(as.matrix(comp), as.matrix(wk_affine_rotate(25)))
})

test_that("wk_affine_compose() can combine translation and rotation", {

  comp <- wk_affine_compose(
    wk_affine_translate(1, 0),
    wk_affine_rotate(90)
  )

  comp_inverse <- wk_affine_compose(
    wk_affine_rotate(-90),
    wk_affine_translate(-1, 0)
  )

  expect_equal(
    as.matrix(wk_affine_invert(comp_inverse)),
    as.matrix(comp)
  )

  # check with actual coordinates
  coords <- matrix(c(0, 0, 1, 1, 2, 2, 3, 3), nrow = 2)
  coords1 <- as.matrix(wk_affine_translate(1, 0)) %*% rbind(coords, 1)
  coords2 <- as.matrix(wk_affine_rotate(90)) %*% coords1

  # the first point will be wrong if the order was backward
  expect_equal(
    as.matrix(comp) %*% rbind(coords, 1)[, 1],
    matrix(c(0, 1, 1), ncol = 1)
  )

  expect_equal(
    as.matrix(comp) %*% rbind(coords, 1),
    coords2
  )

  expect_equal(
    as.matrix(comp_inverse) %*% coords2,
    rbind(coords, 1)
  )
})

test_that("wk_affine_inverse() works", {
  expect_equal(
    as.matrix(wk_affine_compose(wk_affine_rotate(12), wk_affine_translate(1, 2))),
    as.matrix(
      wk_affine_invert(
        wk_affine_compose(wk_affine_translate(-1, -2), wk_affine_rotate(-12))
      )
    )
  )

  expect_equal(
    as.matrix(wk_affine_invert(wk_affine_translate(1, 2))),
    as.matrix(wk_trans_inverse(wk_affine_translate(1, 2)))
  )
})

test_that("wk_transform() works with an affine transformer", {
  expect_equal(
    wk_transform(
      xy(0:3, 0:3),
      wk_affine_identity()
    ),
    xy(0:3, 0:3)
  )

  expect_equal(
    wk_transform(
      xy(0:3, 0:3),
      wk_affine_compose(
        wk_affine_translate(1, 0),
        wk_affine_rotate(90)
      )
    ),
    c(xy(0, 1), xy(-1, 2), xy(-2, 3), xy(-3, 4))
  )
})
