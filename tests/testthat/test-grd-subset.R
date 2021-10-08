
test_that("grd_index() works for grd_rct()", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(grd_index(empty, xy(0, 0)), list(i = NA_integer_, j = NA_integer_))

  grid <- grd_rct(volcano)
  expect_identical(grd_index(grid, xy(NA, NA)), list(i = NA_integer_, j = NA_integer_))

  expect_identical(grd_index(grid, xy(0.5, 0.5)), list(i = 87, j = 1))
  expect_identical(grd_index(grid, xy(0, 87)), list(i = 1, j = 1))
  expect_identical(grd_index(grid, xy(1, 86)), list(i = 2, j = 2))
  expect_identical(grd_index(grid, xy(2, 85)), list(i = 3, j = 3))
})

test_that("grd_index() works for grd_xy()", {
  empty <- grd_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(grd_index(empty, xy(0, 0)), list(i = NA_integer_, j = NA_integer_))

  grid <- grd_xy(volcano)
  expect_identical(grd_index(grid, xy(NA, NA)), list(i = NA_integer_, j = NA_integer_))

  expect_identical(grd_index(grid, xy(0, 0)), list(i = 87, j = 1))
})

test_that("grd_index_range() works for grd_rct()", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(
    grd_index_range(empty, rct(0, 0, 1, 1)),
    list(i = integer(), j = integer())
  )

  grid <- grd_rct(volcano)
  expect_identical(
    grd_index_range(grid, rct(NA, NA, NA, NA)),
    list(i = integer(), j = integer())
  )

  expect_identical(
    grd_index_range(grid, wk_bbox(grid), snap = list(ceiling, floor)),
    list(
      i = c(start = 0, stop = nrow(grid), step = NA_integer_),
      j = c(start = 0, stop = ncol(grid), step = NA_integer_)
    )
  )

  # bbox with exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = rct(0, 86, 3, 87), snap = list(ceiling, floor)),
    list(
      i = c(start = 0, stop = 1, step = NA_integer_),
      j = c(start = 0, stop = 3, step = NA_integer_)
    )
  )

  # subset by bbox with non-exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = rct(0.5, 86.1, 2.5, 86.9)),
    list(
      i = c(start = 0, stop = 1, step = NA_integer_),
      j = c(start = 0, stop = 3, step = NA_integer_)
    )
  )

  # subset by arbitrary object with non-exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = as_wkb(rct(0.5, 86.1, 2.5, 86.9))),
    grd_index_range(grid, bbox = rct(0.5, 86.1, 2.5, 86.9))
  )
})

test_that("grd_index_range() works for grd_xy()", {
  empty <- grd_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(
    grd_index_range(empty, rct(0, 0, 1, 1)),
    list(i = integer(), j = integer())
  )

  grid <- grd_xy(volcano)
  expect_identical(
    grd_index_range(grid, rct(NA, NA, NA, NA)),
    list(i = integer(), j = integer())
  )

  expect_identical(
    grd_index_range(grid, wk_bbox(grid)),
    list(
      i = c(start = 0, stop = nrow(grid), step = NA_integer_),
      j = c(start = 0, stop = ncol(grid), step = NA_integer_)
    )
  )

  # bbox with exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = rct(0, 85, 3, 86)),
    list(
      i = c(start = 0, stop = 2, step = NA_integer_),
      j = c(start = 0, stop = 4, step = NA_integer_)
    )
  )

  # subset by bbox with non-exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = rct(0.6, 85.9, 2.4, 86.1)),
    list(
      i = c(start = 0, stop = 1, step = NA_integer_),
      j = c(start = 1, stop = 3, step = NA_integer_)
    )
  )

  # subset by arbitrary object with non-exact boundaries
  expect_identical(
    grd_index_range(grid, bbox = as_wkb(rct(0.5, 86.1, 2.5, 86.9))),
    grd_index_range(grid, bbox = rct(0.5, 86.1, 2.5, 86.9))
  )
})

test_that("grd_cell_bounds() works for grd_rct()", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(
    wk_bbox(grd_cell_bounds(empty, 0, 0)),
    wk_bbox(rct(NA, NA, NA, NA))
  )

  grid <- grd(nx = 3, ny = 2)
  expect_identical(grd_cell_bounds(grid, 1, 1), rct(0, 1, 1, 2))
  expect_identical(grd_cell_bounds(grid, 0, 0), rct(-1, 2, 0, 3))
  expect_identical(
    grd_cell_bounds(grid, 0, 0, out_of_bounds = "discard"),
    rct(crs = NULL)
  )
  expect_identical(
    wk_bbox(grd_cell_bounds(grid, 0, 0, out_of_bounds = "censor")),
    wk_bbox(rct(NA, NA, NA, NA))
  )
  expect_identical(
    grd_cell_bounds(grid, 0, 0, out_of_bounds = "squish"),
    grd_cell_bounds(grid, 1, 1)
  )
})

test_that("grd_cell_bounds() works for grd_xy()", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(
    wk_bbox(grd_cell_bounds(empty, 0, 0)),
    wk_bbox(rct(NA, NA, NA, NA))
  )

  grid <- grd(nx = 3, ny = 2, type = "centers")
  expect_identical(grd_cell_bounds(grid, 1, 1), rct(0, 1, 1, 2))
  expect_identical(grd_cell_bounds(grid, 0, 0), rct(-1, 2, 0, 3))
  expect_identical(
    grd_cell_bounds(grid, 0, 0, out_of_bounds = "discard"),
    rct(crs = NULL)
  )
  expect_identical(
    wk_bbox(grd_cell_bounds(grid, 0, 0, out_of_bounds = "censor")),
    wk_bbox(rct(NA, NA, NA, NA))
  )
  expect_identical(
    grd_cell_bounds(grid, 0, 0, out_of_bounds = "squish"),
    grd_cell_bounds(grid, 1, 1)
  )
})

test_that("grd_cell_center() works for grd_rct()", {
  empty <- grd_rct(matrix(nrow = 0, ncol = 0))
  expect_identical(
    grd_cell_center(empty, 0, 0),
    xy(NA, NA)
  )

  grid <- grd(nx = 3, ny = 2)
  expect_identical(grd_cell_center(grid, 1, 1), xy(0.5, 1.5))
  expect_identical(grd_cell_center(grid, 0, 0), xy(-0.5, 2.5))
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "discard"),
    xy(crs = NULL)
  )
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "censor"),
    xy(NA, NA)
  )
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "squish"),
    grd_cell_center(grid, 1, 1)
  )
})

test_that("grd_cell_center() works for grd_xy()", {
  empty <- grd_xy(matrix(nrow = 0, ncol = 0))
  expect_identical(
    grd_cell_center(empty, 0, 0),
    xy(NA, NA)
  )

  grid <- grd(nx = 3, ny = 2, type = "centers")
  expect_identical(grd_cell_center(grid, 1, 1), xy(0.5, 1.5))
  expect_identical(grd_cell_center(grid, 0, 0), xy(-0.5, 2.5))
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "discard"),
    xy(crs = NULL)
  )
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "censor"),
    xy(NA, NA)
  )
  expect_identical(
    grd_cell_center(grid, 0, 0, out_of_bounds = "squish"),
    grd_cell_center(grid, 1, 1)
  )
})

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

test_that("ij_expand_one works", {
  expect_identical(ij_expand_one(NULL, 0L), integer())
  expect_identical(ij_expand_one(NULL, 2L), 1:2)
  expect_identical(ij_expand_one(4:8, 10L), 4:8)
  expect_identical(ij_expand_one(c(start = NA, stop = NA, step = NA), 10L), 1:10)
  expect_identical(ij_expand_one(c(start = 0L, stop = 4L, step = NA), 10L), 1:4)
  expect_identical(ij_expand_one(c(start = 0L, stop = 4L, step = 2L), 10L), c(1L, 3L))

  expect_identical(ij_expand_one(0:2, 1L, out_of_bounds = "keep"), 0:2)
  expect_identical(ij_expand_one(0:2, 1L, out_of_bounds = "censor"), c(NA, 1L, NA))
  expect_identical(ij_expand_one(0:2, 1L, out_of_bounds = "discard"), 1L)
  expect_identical(ij_expand_one(0:2, 1L, out_of_bounds = "squish"), c(1L, 1L, 1L))
})

test_that("ij_to_slice_one works", {
  expect_identical(ij_to_slice_one(NULL, 0L), integer())
  expect_identical(ij_to_slice_one(NULL, 2L), c(start = 0L, stop = 2L, step = 1L))
  expect_identical(ij_to_slice_one(integer(), 2L), integer())
  expect_identical(ij_to_slice_one(1L, 2L), c(start = 0L, stop = 1L, step = 1L))
  expect_identical(ij_to_slice_one(4:8, 10L), c(start = 3L, stop = 8L, step = 1L))
  expect_identical(
    ij_to_slice_one(seq(1L, 9L, by = 2L), 10L),
    c(start = 0L, stop = 9L, step = 2L)
  )
  expect_identical(
    ij_to_slice_one(c(start = NA, stop = NA, step = NA), 10L),
    c(start = 0L, stop = 10L, step = 1L)
  )
  expect_identical(
    ij_to_slice_one(c(start = 1L, stop = 2L, step = 3L), 10L),
    c(start = 1L, stop = 2L, step = 3L)
  )

  expect_error(
    ij_to_slice_one(c(1, 2, 4), 1L),
    "must be equally spaced"
  )

  expect_error(
    ij_to_slice_one(c(1, 0, -1), 1L),
    "must be equally spaced and ascending"
  )

  expect_error(
    ij_to_slice_one(NA_integer_, 1L),
    "must be finite"
  )

  expect_error(
    ij_to_slice_one(logical(), 1L),
    "must be NULL, numeric, or"
  )
})

test_that("snap functions work as expected", {
  expect_identical(grd_snap_next(seq(0, 1, 0.25)), c(0, 0, 1, 1, 1))
  expect_identical(grd_snap_previous(seq(0, 1, 0.25)), c(0, 0, 0, 1, 1))
})
