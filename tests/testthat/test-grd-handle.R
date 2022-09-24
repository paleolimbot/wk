
test_that("wk_handle() works for grd_rct", {
  expect_identical(
    wk_handle(grd(nx = 1, ny = 1), wkt_writer()),
    wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))")
  )
})

test_that("wk_handle() works for grd_xy", {
  expect_identical(
    wk_handle(grd(nx = 1, ny = 1, type = "centers"), wkt_writer()),
    wkt("POINT (0.5 0.5)")
  )
})

test_that("as_xy() works for grd objects", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(as_xy(grid_empty), xy(crs = NULL))

  grid1 <- grd(nx = 1, ny = 1)
  expect_identical(as_xy(grid1), xy(0.5, 0.5))

  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_xy(data)

  # order should match the internal ordering of data
  # (column major unless specified)
  expect_identical(
    as_xy(grd_xy(data)),
    c(
      xy(0, 1),
      xy(0, 0),
      xy(1, 1),
      xy(1, 0),
      xy(2, 1),
      xy(2, 0)
    )
  )

  # order should still be top left -> bottom right
  # even with flipped initial bbox
  expect_identical(
    as_xy(grd_xy(data, rct(0, 0, -2, -1))),
    c(
      xy(-2, 0),
      xy(-2, -1),
      xy(-1, 0),
      xy(-1, -1),
      xy(0, 0),
      xy(0, -1)
    )
  )

  expect_identical(as_xy(as_grd_rct(grid)), as_xy(grid))
})

test_that("as_xy() works for row-major grd objects", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(as_xy(grid_empty, data_order = c("x", "y")), xy(crs = NULL))

  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_xy(data)

  expect_identical(
    as_xy(grid, data_order = c("x", "y")),
    c(
      xy(0, 1),
      xy(1, 1),
      xy(2, 1),
      xy(0, 0),
      xy(1, 0),
      xy(2, 0)
    )
  )
})

test_that("as_xy() works for flipped grd objects", {
  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_xy(data)

  expect_identical(
    as_xy(grid, data_order = c("-y", "-x")),
    c(
      xy(2, 0),
      xy(2, 1),
      xy(1, 0),
      xy(1, 1),
      xy(0, 0),
      xy(0, 1)
    )
  )

  expect_identical(
    as_xy(grid, data_order = c("-x", "-y")),
    c(
      xy(2, 0),
      xy(1, 0),
      xy(0, 0),
      xy(2, 1),
      xy(1, 1),
      xy(0, 1)
    )
  )
})

test_that("as_rct() works for grd objects", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(as_rct(grid_empty), rct(crs = NULL))

  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_rct(data)

  # order should match the internal ordering of data
  # (column major unless specified)
  expect_identical(
    as_rct(grid),
    c(
      rct(0, 1, 1, 2),
      rct(0, 0, 1, 1),
      rct(1, 1, 2, 2),
      rct(1, 0, 2, 1),
      rct(2, 1, 3, 2),
      rct(2, 0, 3, 1)
    )
  )

  expect_identical(
    as_rct(grd_rct(data, rct(0, 0, -3, -2))),
    c(
      rct(-3, -1, -2, 0),
      rct(-3, -2, -2, -1),
      rct(-2, -1, -1, 0),
      rct(-2, -2, -1, -1),
      rct(-1, -1, 0, 0),
      rct(-1, -2, 0, -1)
    )
  )

  expect_identical(as_rct(as_grd_xy(grid)), as_rct(grid))
})

test_that("as_rct() works for row-major grd objects", {
  grid_empty <- grd(nx = 0, ny = 0)
  expect_identical(as_rct(grid_empty, data_order = c("x", "y")), rct(crs = NULL))

  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_rct(data)

  # order should match the internal ordering of data
  # (row major unless specified)
  expect_identical(
    as_rct(grid, data_order = c("x", "y")),
    c(
      rct(0, 1, 1, 2),
      rct(1, 1, 2, 2),
      rct(2, 1, 3, 2),
      rct(0, 0, 1, 1),
      rct(1, 0, 2, 1),
      rct(2, 0, 3, 1)
    )
  )

  expect_identical(as_rct(as_grd_xy(grid)), as_rct(grid))
})

test_that("as_rct() works for flipped grd objects", {
  data <- matrix(0:5, nrow = 2, ncol = 3)
  grid <- grd_rct(data)

  # order should match the internal ordering of data
  # (row major unless specified)
  expect_identical(
    as_rct(grid, data_order = c("-y", "-x")),
    c(
      rct(2, 0, 3, 1),
      rct(2, 1, 3, 2),
      rct(1, 0, 2, 1),
      rct(1, 1, 2, 2),
      rct(0, 0, 1, 1),
      rct(0, 1, 1, 2)
    )
  )

  # order should match the internal ordering of data
  # (row major unless specified)
  expect_identical(
    as_rct(grid, data_order = c("-x", "-y")),
    c(
      rct(2, 0, 3, 1),
      rct(1, 0, 2, 1),
      rct(0, 0, 1, 1),
      rct(2, 1, 3, 2),
      rct(1, 1, 2, 2),
      rct(0, 1, 1, 2)
    )
  )
})
