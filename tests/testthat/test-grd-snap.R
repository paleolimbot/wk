
test_that("snap functions work as expected", {
  expect_identical(grd_snap_next(seq(0, 1, 0.25)), c(0, 0, 1, 1, 1))
  expect_identical(grd_snap_previous(seq(0, 1, 0.25)), c(0, 0, 0, 1, 1))
})
