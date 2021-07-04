
test_that("wk_trans_affine() works", {
  expect_s3_class(wk_affine_identity(), "wk_trans_affine")
  expect_output(print(wk_affine_identity()), "wk_trans_affine")
})

test_that("wk_trans_affine() errors for invalid matrix", {
  expect_error(wk_trans_affine(5), "must be a 3x3 matrix")
})

test_that("wk_affine_compose() works", {
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

test_that("wk_affine_inverse() works", {
  expect_equal(
    as.matrix(wk_affine_compose(wk_affine_rotate(12), wk_affine_translate(1, 2))),
    as.matrix(
      wk_affine_invert(
        wk_affine_compose(wk_affine_translate(-1, -2), wk_affine_rotate(-12))
      )
    )
  )
})
