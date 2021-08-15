
test_that("wk_grid_rct() works", {
  grid <- wk_grid_rct(volcano)
  expect_s3_class(grid, "wk_grid_rct")
  expect_s3_class(grid, "wk_grid")
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 87, 61))

  expect_match(format(grid), "wk_grid_rct")
  expect_output(print(grid), "wk_grid_rct")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grid_rct")
  expect_output(print(grid_crs), "wk_grid_rct")
})

test_that("wk_grid_xy() works", {
  grid <- wk_grid_xy(volcano)
  expect_s3_class(grid, "wk_grid_xy")
  expect_s3_class(grid, "wk_grid")
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 87, 61))

  expect_match(format(grid), "wk_grid_xy")
  expect_output(print(grid), "wk_grid_xy")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grid_xy")
  expect_output(print(grid_crs), "wk_grid_xy")
})

