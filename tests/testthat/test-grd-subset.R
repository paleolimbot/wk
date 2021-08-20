
test_that("subset works for grd_rct", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(grd_subset(empty), empty)

  grid <- grd_rct(volcano)

  # ways to identity subset
  expect_identical(grd_subset(grid), grid)
  expect_identical(grd_subset(grid, bbox = wk_bbox(grid)), grid)
  expect_identical(grd_subset(grid, 1:87, 1:61), grid)
  expect_identical(grd_subset(grid, NULL, 1:61), grid)
  expect_identical(grd_subset(grid, 1:87, NULL), grid)

  # bad args
  expect_error(grd_subset(grid, raw(), NULL), "must be NULL, numeric, or logical")
  expect_error(grd_subset(grid, NULL, raw()), "must be NULL, numeric, or logical")
  expect_error(grd_subset(grid, T, T, rct()), "Must specify")

  # check small subsets for exactness
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

  # subset by logical
  expect_identical(
    grd_subset(grid, c(rep(T, 2), rep(F, 85)), c(rep(T, 3), rep(F, 58))),
    grd_subset(grid, 1:2, 1:3)
  )

  # subset by bbox with exact boundaries
  expect_identical(
    grd_subset(grid, bbox = rct(0, 86, 3, 87)),
    grd_subset(grid, x = 1:3, y = 1)
  )

  # subset by bbox with non-exact boundaries
  expect_identical(
    grd_subset(grid, bbox = rct(0.5, 86.1, 2.5, 86.9)),
    grd_subset(grid, x = 1:3, y = 1)
  )
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
  grid_21 <- grd_subset(grid_native, y = 2, x = 2:3)
  expect_identical(
    as.integer(grid_21$data),
    c(-11711155L, -8355712L)
  )
})
