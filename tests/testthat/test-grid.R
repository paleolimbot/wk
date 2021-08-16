
test_that("wk_grid_rct() works", {
  grid <- wk_grid_rct(volcano)
  expect_s3_class(grid, "wk_grid_rct")
  expect_s3_class(grid, "wk_grid")
  expect_identical(as_wk_grid_rct(grid), grid)
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 87, 61))

  expect_match(format(grid), "wk_grid_rct")
  expect_output(print(grid), "wk_grid_rct")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grid_rct")
  expect_output(print(grid_crs), "wk_grid_rct")
})

test_that("wk_grid_rct() works for an empty grid", {
  empty <- wk_grid_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(wk_bbox(empty), wk_bbox(xy(crs = NULL)))
})

test_that("wk_grid_xy() works for an empty grid", {
  empty <- wk_grid_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(wk_bbox(empty), wk_bbox(xy(crs = NULL)))
})

test_that("wk_grid_xy() works for h/v lines", {
  hline <- wk_grid_xy(matrix(nrow = 10, ncol = 1), rct(0, 0, 1, 0))
  expect_identical(wk_bbox(hline), rct(0, 0, 1, 0))
  vline <- wk_grid_xy(matrix(nrow = 1, ncol = 10), rct(0, 0, 0, 1))
  expect_identical(wk_bbox(vline), rct(0, 0, 0, 1))
})

test_that("wk_grid_xy() works", {
  grid <- wk_grid_xy(volcano)
  expect_s3_class(grid, "wk_grid_xy")
  expect_s3_class(grid, "wk_grid")
  expect_identical(as_wk_grid_xy(grid), grid)
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 87, 61))

  expect_match(format(grid), "wk_grid_xy")
  expect_output(print(grid), "wk_grid_xy")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grid_xy")
  expect_output(print(grid_crs), "wk_grid_xy")
})


test_that("wk_grid() works", {
  grid_nxny <- wk_grid(nx = 10, ny = 20, type = "polygons")
  expect_identical(dim(grid_nxny$data), c(10L, 20L, 0L))
  expect_identical(wk_bbox(grid_nxny), rct(0, 0, 10, 20))

  grid_dxdy <- wk_grid(rct(0, 0, 10, 20), dx = 1, dy = 4)
  expect_identical(dim(grid_dxdy$data), c(10L, 5L, 0L))
  expect_identical(wk_bbox(grid_dxdy), rct(0, 0, 10, 20))

  center_nxny <- wk_grid(nx = 10, ny = 20, type = "centers")
  expect_identical(dim(center_nxny$data), c(10L, 20L, 0L))
  expect_identical(wk_bbox(center_nxny), rct(0.5, 0.5, 9.5, 19.5))

  center_dxdy <- wk_grid(rct(0, 0, 10, 20), dx = 1, dy = 4, type = "centers")
  expect_identical(dim(center_dxdy$data), c(10L, 5L, 0L))
  expect_identical(wk_bbox(center_dxdy), rct(0.5, 2, 9.5, 18))
})
