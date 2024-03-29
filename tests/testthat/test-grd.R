
test_that("grd_rct() works", {
  grid <- grd_rct(volcano)
  expect_s3_class(grid, "wk_grd_rct")
  expect_s3_class(grid, "wk_grd")
  expect_identical(as_grd_rct(grid), grid)
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 61, 87))

  expect_match(format(grid), "wk_grd_rct")
  expect_output(print(grid), "wk_grd_rct")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grd_rct")
  expect_output(print(grid_crs), "wk_grd_rct")
})

test_that("grd_rct() works for an empty grid", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(wk_bbox(empty), wk_bbox(xy(crs = NULL)))
})

test_that("grd_xy() works for an empty grid", {
  empty <- grd_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(wk_bbox(empty), wk_bbox(xy(crs = NULL)))
})

test_that("grd_xy() works for h/v lines", {
  hline <- grd_xy(matrix(nrow = 10, ncol = 1), rct(0, 0, 0, 1))
  expect_identical(wk_bbox(hline), rct(0, 0, 0, 1))
  vline <- grd_xy(matrix(nrow = 1, ncol = 10), rct(0, 0, 1, 0))
  expect_identical(wk_bbox(vline), rct(0, 0, 1, 0))
})

test_that("grd_xy() works", {
  grid <- grd_xy(volcano)
  expect_s3_class(grid, "wk_grd_xy")
  expect_s3_class(grid, "wk_grd")
  expect_identical(as_grd_xy(grid), grid)
  expect_null(wk_crs(grid))
  expect_identical(wk_bbox(grid), rct(0, 0, 60, 86))

  expect_match(format(grid), "wk_grd_xy")
  expect_output(print(grid), "wk_grd_xy")

  grid_crs <- wk_set_crs(grid, 1234)
  expect_match(format(grid_crs), "wk_grd_xy")
  expect_output(print(grid_crs), "wk_grd_xy")
})

test_that("grd_xy <-> grd_rct converters work", {
  grid <- grd_rct(volcano)
  expect_identical(as_grd_rct(as_grd_xy(grid)), grid)

  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(as_grd_rct(as_grd_xy(empty)), empty)

  # make sure x/y value don't get lost for a single row/col
  expect_identical(
    wk_bbox(as_grd_rct(as_grd_xy(grd(nx = 10, ny = 1)))),
    rct(0, 0.5, 10, 0.5)
  )
  expect_identical(
    wk_bbox(as_grd_rct(as_grd_xy(grd(nx = 1, ny = 10)))),
    rct(0.5, 0, 0.5, 10)
  )
})

test_that("grd() works", {
  grid_bbox <- grd(
    bbox = xy(c(0, 10), c(0, 20)),
    nx = 10, ny = 20, type = "polygons"
  )
  expect_identical(dim(grid_bbox$data), c(20L, 10L, 0L))
  expect_identical(wk_bbox(grid_bbox), rct(0, 0, 10, 20))

  grid_nxny <- grd(nx = 10, ny = 20, type = "polygons")
  expect_identical(dim(grid_nxny$data), c(20L, 10L, 0L))
  expect_identical(wk_bbox(grid_nxny), rct(0, 0, 10, 20))

  grid_dxdy <- grd(rct(0, 0, 10, 20), dx = 1, dy = 4)
  expect_identical(dim(grid_dxdy$data), c(5L, 10L, 0L))
  expect_identical(wk_bbox(grid_dxdy), rct(0, 0, 10, 20))

  center_nxny <- grd(nx = 10, ny = 20, type = "centers")
  expect_identical(dim(center_nxny$data), c(20L, 10L, 0L))
  expect_identical(wk_bbox(center_nxny), rct(0.5, 0.5, 9.5, 19.5))

  center_dxdy <- grd(rct(0, 0, 10, 20), dx = 1, dy = 4, type = "centers")
  expect_identical(dim(center_dxdy$data), c(5L, 10L, 0L))
  expect_identical(wk_bbox(center_dxdy), rct(0.5, 2, 9.5, 18))

  corner_nxny <- grd(nx = 10, ny = 20, type = "corners")
  expect_identical(dim(corner_nxny$data), c(21L, 11L, 0L))
  expect_identical(wk_bbox(corner_nxny), rct(0, 0, 10, 20))

  corner_dxdy <- grd(rct(0, 0, 10, 20), dx = 1, dy = 4, type = "corners")
  expect_identical(dim(corner_dxdy$data), c(6L, 11L, 0L))
  expect_identical(wk_bbox(corner_dxdy), rct(0, 0, 10, 20))

  expect_error(grd(), "Must specify")
})

test_that("grd matrix interface works", {
  grid <- grd_rct(array(1:24, dim = c(2, 3, 4)))
  expect_identical(grid[1, 1, ], grd_subset(grid, 1, 1))
  expect_identical(grid[, , 1], grd_rct(grid$data[, , 1, drop = FALSE]))
  expect_identical(dim(grid)[1], 2L)
  expect_identical(dim(grid)[2], 3L)

  # rct subsetting
  expect_identical(grid[rct(0.1, 1.1, 0.9, 1.9), ], grd_subset(grid, 1, 1))
  expect_equal(unname(dim(grid[rct(0.1, 1.1, 0.9, 1.9), 1])), c(1L, 1L, 1L))
  grid$data <- grid$data[, , 1, drop = TRUE]
  expect_identical(grid[rct(0.1, 1.1, 0.9, 1.9), ], grd_subset(grid, 1, 1))
})

test_that("grd[[]]<- interface works", {
  grid <- grd_rct(array(1:24, dim = c(2, 3, 4)))

  grid[["bbox"]] <- rct(0, 0, 1, 1)
  expect_identical(wk_bbox(grid), rct(0, 0, 1, 1))

  # make sure this is normalized/converted
  grid[["bbox"]] <- rct(1, 1, 0, 0)
  expect_identical(wk_bbox(grid), rct(0, 0, 1, 1))

  grid[["bbox"]] <- as_wkb(rct(1, 1, 0, 0))
  expect_identical(wk_bbox(grid), rct(0, 0, 1, 1))

  expect_error(grid[["bbox"]] <- rct(NA, 1, 0, 0), "Can't set missing")

  grid[["data"]] <- matrix()
  expect_identical(grid, grd_rct(matrix(), rct(0, 0, 1, 1)))

  expect_error(grid[["not_data_or_bbox"]] <- NULL, "Can't set element")
})

test_that("grd$<- interface works", {
  grid <- grd_rct(array(1:24, dim = c(2, 3, 4)))

  grid$bbox <- rct(0, 0, 1, 1)
  expect_identical(wk_bbox(grid), rct(0, 0, 1, 1))

  grid$data <- matrix()
  expect_identical(grid, grd_rct(matrix(), rct(0, 0, 1, 1)))

  expect_error(grid$not_data_or_bbox <- NULL, "Can't set element")
})
