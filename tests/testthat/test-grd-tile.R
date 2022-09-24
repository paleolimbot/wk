
test_that("grd_overview_summary() works", {
  grid <- grd_rct(volcano)
  summary <- grd_overview_summary(grid)

  # 0th overview is the same as the data
  expect_identical(summary$nx[1], ncol(grid))
  expect_identical(summary$ny[1], nrow(grid))

  # Last overview is 1 x 1
  expect_identical(summary$nx[8], 1L)
  expect_identical(summary$ny[8], 1L)
})
