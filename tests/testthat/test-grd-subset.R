
test_that("subset works for grd_rct", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(grd_subset(empty), empty)

  grid <- grd_rct(volcano)

  # ways to identity subset
  expect_identical(grd_subset(grid), grid)
  expect_identical(grd_subset(grid, 1:87, 1:61), grid)
  expect_identical(grd_subset(grid, NULL, 1:61), grid)
  expect_identical(grd_subset(grid, 1:87, NULL), grid)

  # bad args
  expect_error(grd_subset(grid, raw(), NULL), "must be NULL, numeric, or")
  expect_error(grd_subset(grid, NULL, raw()), "must be NULL, numeric, or")

  # check small subsets for exactness
  grid_00 <- grd_subset(grid, integer(), integer())
  expect_identical(grid_00$data, volcano[integer(), integer()])
  expect_identical(wk_bbox(grid_00), rct(Inf, Inf, -Inf, -Inf))

  grid_11 <- grd_subset(grid, 2, 2)
  expect_identical(grid_11$data, volcano[2, 2, drop = FALSE])
  expect_identical(wk_bbox(grid_11), rct(1, 85, 2, 86))

  grid_23 <- grd_subset(grid, 1:2, 1:3)
  expect_identical(grid_23$data, volcano[1:2, 1:3])
  expect_identical(wk_bbox(grid_23), rct(0, 85, 3, 87))

  grid_13 <- grd_subset(grid, 1, 1:3)
  expect_identical(grid_13$data, volcano[1, 1:3, drop = FALSE])
  expect_identical(wk_bbox(grid_13), rct(0, 86, 3, 87))

  grid_03 <- grd_subset(grid, integer(), 1:3)
  expect_identical(grid_03$data, volcano[integer(), 1:3, drop = FALSE])
  expect_identical(grid_03$bbox, rct(0, Inf, 3, -Inf))

  grid_31 <- grd_subset(grid, 1:3, 1)
  expect_identical(grid_31$data, volcano[1:3, 1, drop = FALSE])
  expect_identical(wk_bbox(grid_31), rct(0, 84, 1, 87))

  grid_30 <- grd_subset(grid, 1:3, integer())
  expect_identical(grid_30$data, volcano[1:3, integer(), drop = FALSE])
  expect_identical(grid_30$bbox, rct(Inf, 84, -Inf, 87))
})

test_that("grd_subset() works for a grd_rct backed by nativeRaster", {
  # can aso check with PNG
  # col_native <- png::readPNG(system.file("img", "Rlogo.png", package="png"), native = T)
  # grid_native <- grd_rct(col_native)
  # plot(grid_native)
  # plot(grd_subset(grid_native, bbox = rct(20, 40, 60, 60)), border = T)

  col_native <- structure(
    c(-16777216L, -13421773L, -10066330L, -15066598L, -11711155L, -8355712L),
    .Dim = 2:3,
    class = "nativeRaster"
  )

  grid_native <- grd_rct(col_native)
  grid_21 <- grd_subset(grid_native, i = 2, j = 2:3)
  expect_identical(
    as.integer(grid_21$data),
    c(-11711155L, -8355712L)
  )
})

test_that("grd_subset() preserves dimensions for nd arrays", {
  grid <- grd_rct(array(1:24, dim = c(2, 3, 4)))

  expect_identical(
    grd_subset(grid, 1, 1),
    grd_rct(array(c(1L, 7L, 13L, 19L), dim = c(1, 1, 4)), bbox = rct(0, 1, 1, 2))
  )
})

test_that("subset works for grd_xy", {
  empty <- grd_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(grd_subset(empty), empty)

  grid <- grd_xy(volcano)

  # ways to identity subset
  expect_identical(grd_subset(grid), grid)
  expect_identical(grd_subset(grid, 1:87, 1:61), grid)
  expect_identical(grd_subset(grid, NULL, 1:61), grid)
  expect_identical(grd_subset(grid, 1:87, NULL), grid)

  # check small subsets for exactness
  grid_00 <- grd_subset(grid, integer(), integer())
  expect_identical(grid_00$data, volcano[integer(), integer()])
  expect_identical(wk_bbox(grid_00), rct(Inf, Inf, -Inf, -Inf))

  grid_11 <- grd_subset(grid, 2, 2)
  expect_identical(grid_11$data, volcano[2, 2, drop = FALSE])
  expect_identical(wk_bbox(grid_11), rct(1, 85, 1, 85))

  grid_23 <- grd_subset(grid, 1:2, 1:3)
  expect_identical(grid_23$data, volcano[1:2, 1:3])
  expect_identical(wk_bbox(grid_23), rct(0, 85, 2, 86))
})

test_that("crop works for grd_rct and grd_xy", {
  grid <- grd(nx = 3, ny = 2)
  expect_identical(grd_crop(grid, grid$bbox), grid)
  expect_identical(
    grd_crop(grid, rct(0, 0, 2, 2)),
    grd_subset(grid, 1:2, 1:2)
  )
  expect_identical(
    grd_crop(grid, rct(-1, -1, 2, 2)),
    grd_subset(grid, 1:2, 1:2)
  )

  grid <- grd(nx = 3, ny = 2, type = "corners")
  expect_identical(grd_crop(grid, grid$bbox), grid)
  expect_identical(
    grd_crop(grid, rct(0, 0, 2, 2)),
    grd_subset(grid, 1:3, 1:3)
  )
  expect_identical(
    grd_crop(grid, rct(-1, -1, 2, 2)),
    grd_subset(grid, 1:3, 1:3)
  )
})

test_that("crop/extend works for grd_rct", {
  grid <- grd(nx = 3, ny = 2)
  expect_identical(grd_extend(grid, grid$bbox), grid)
  expect_identical(
    grd_extend(grid, rct(0, 0, 2, 2)),
    grd_subset(grid, 1:2, 1:2)
  )
  expect_identical(
    grd_extend(grid, rct(-1, -1, 2, 2)),
    grd_subset(grid, 1:3, 0:2)
  )

  grid <- grd(nx = 3, ny = 2, type = "corners")
  expect_identical(grd_extend(grid, grid$bbox), grid)
  expect_identical(
    grd_extend(grid, rct(0, 0, 2, 2)),
    grd_subset(grid, 1:3, 1:3)
  )
  expect_identical(
    grd_extend(grid, rct(-1, -1, 2, 2)),
    grd_subset(grid, 1:4, 0:3)
  )
})

test_that("crop/extend works for grd_xy", {
  grid <- grd(nx = 2, ny = 1, type = "corners")
  expect_identical(grd_crop(grid, grid$bbox), grid)
})
