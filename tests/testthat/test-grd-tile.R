
test_that("grd_tile_summary() works", {
  grid <- grd_rct(volcano)
  summary <- grd_tile_summary(grid)

  # 0th overview is the same as the data
  expect_identical(summary$nx[1], ncol(grid))
  expect_identical(summary$ny[1], nrow(grid))

  # Last overview is 1 x 1
  expect_identical(summary$nx[8], 1L)
  expect_identical(summary$ny[8], 1L)
})

test_that("grd_tile_template() works for perfectly tileable rct data", {
  grid <- grd(nx = 8, ny = 8)
  grid$data <- matrix(1:64, nrow = 8, ncol = 8, byrow = TRUE)

  expect_identical(
    grd_tile_template(grid, 1),
    grd(grid$bbox, nx = 4, ny = 4)
  )
})

test_that("grd_tile_template() works for imperfectly tileable rct data", {
  grid <- grd(nx = 9, ny = 9)
  grid$data <- matrix(1:81, nrow = 9, ncol = 9, byrow = TRUE)

  expect_identical(
    grd_tile_template(grid, 1),
    grd(rct(0, -1, 10, 9), nx = 5, ny = 5)
  )
})

test_that("grd_tile_template() works for perfectly tileable xy data", {
  grid <- grd(nx = 8, ny = 8, type = "centers")
  grid$data <- matrix(1:64, nrow = 8, ncol = 8, byrow = TRUE)

  expect_identical(
    grd_tile_template(grid, 1),
    grd(rct(0, 0, 8, 8), nx = 4, ny = 4)
  )
})

test_that("grd_tile_template() works for imperfectly tileable xy data", {
  grid <- grd(nx = 9, ny = 9, type = "centers")
  grid$data <- matrix(1:81, nrow = 9, ncol = 9, byrow = TRUE)

  expect_identical(
    grd_tile_template(grid, 1),
    grd(rct(0, -1, 10, 9), nx = 5, ny = 5)
  )
})

test_that("grd_tile() works for perfectly tileable rct data", {
  grid <- grd(nx = 8, ny = 8)
  grid$data <- matrix(1:64, nrow = 8, ncol = 8, byrow = TRUE)

  # top-left
  tile11 <- grd_tile(grid, 1, 1, 1)
  expect_identical(tile11$bbox, rct(0, 6, 2, 8))
  expect_identical(tile11$data, matrix(c(1L, 9L, 2L, 10L), nrow = 2, ncol = 2))

  # bottom right
  tile44 <- grd_tile(grid, 1, 4, 4)
  expect_identical(tile44$bbox, rct(6, 0, 8, 2))
  expect_identical(tile44$data, matrix(c(55L, 63L, 56L, 64L), nrow = 2, ncol = 2))
})

test_that("grd_tile() works for imperfectly tileable rct data", {
  grid <- grd(nx = 9, ny = 9)
  grid$data <- matrix(1:81, nrow = 9, ncol = 9, byrow = TRUE)

  # top-left
  tile11 <- grd_tile(grid, 1, 1, 1)
  expect_identical(tile11$bbox, rct(0, 7, 2, 9))
  expect_identical(tile11$data, matrix(c(1L, 10L, 2L, 11L), nrow = 2, ncol = 2))

  # almost bottom right
  tile45 <- grd_tile(grid, 1, 4, 5)
  expect_identical(tile45$bbox, rct(8, 1, 10, 3))
  expect_identical(tile45$data, matrix(c(63L, 72L, NA, NA), nrow = 2, ncol = 2))

  # bottom almost right
  tile54 <- grd_tile(grid, 1, 5, 4)
  expect_identical(tile54$bbox, rct(6, -1, 8, 1))
  expect_identical(tile54$data, matrix(c(79L, NA, 80L, NA), nrow = 2, ncol = 2))
})

test_that("grd_tile() works for perfectly tileable rct data", {
  grid <- grd(nx = 8, ny = 8, type = "centers")
  grid$data <- matrix(1:64, nrow = 8, ncol = 8, byrow = TRUE)

  # top-left
  tile11 <- grd_tile(grid, 1, 1, 1)
  expect_s3_class(tile11, "grd_xy")
  expect_identical(tile11$bbox, rct(0.5, 6.5, 1.5, 7.5))
  expect_identical(tile11$data, matrix(c(1L, 9L, 2L, 10L), nrow = 2, ncol = 2))

  # bottom right
  tile44 <- grd_tile(grid, 1, 4, 4)
  expect_s3_class(tile11, "grd_xy")
  expect_identical(tile44$bbox, rct(6.5, 0.5, 7.5, 1.5))
  expect_identical(tile44$data, matrix(c(55L, 63L, 56L, 64L), nrow = 2, ncol = 2))
})
