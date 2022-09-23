
test_that("grd_data_generic() works for the column-major case", {
  data <- array(0:5, dim = c(2, 3, 1))

  data_g <- grd_data_generic(data, data_order = c("y", "x", NA))
  expect_identical(dim(data_g), dim(data))
  expect_identical(grd_data_collect(data_g), data)

  expect_identical(
    data_g[1:2, 1, ],
    grd_data_generic(data[1:2, 1, , drop = FALSE])
  )
  expect_identical(
    data_g[1:2, , ],
    grd_data_generic(data[1:2, , , drop = FALSE])
  )
  expect_identical(
    data_g[, 1, ],
    grd_data_generic(data[, 1, , drop = FALSE])
  )
})

test_that("grd_data_generic() works for the row-major case", {
  data <- array(0:5, dim = c(2, 3, 1))

  data_g <- grd_data_generic(data, data_order = c("x", "y", NA))
  expect_identical(dim(data_g), c(3L, 2L, 1L))
  expect_identical(grd_data_collect(data_g), aperm(data, c(2, 1, 3)))

  expect_identical(
    data_g[1:2, 1, ]$grid_data,
    aperm(aperm(data, c(2, 1, 3))[1:2, 1, , drop = FALSE], c(2, 1, 3))
  )
  expect_identical(
    data_g[1:2, , ]$grid_data,
    aperm(aperm(data, c(2, 1, 3))[1:2, , , drop = FALSE], c(2, 1, 3))
  )
  expect_identical(
    data_g[, 1, ]$grid_data,
    aperm(aperm(data, c(2, 1, 3))[, 1, , drop = FALSE], c(2, 1, 3))
  )
})

test_that("grd_data_generic() works for the flipped case", {
  data <- array(0:5, dim = c(2, 3, 1))

  data_g <- grd_data_generic(data, data_order = c("-y", "-x", NA))
  expect_identical(dim(data_g), dim(data))
  expect_identical(grd_data_collect(data_g), data[2:1, 3:1, , drop = FALSE])

  expect_identical(data_g[1, 1, ]$grid_data, array(5L, dim = c(1, 1, 1)))
  expect_identical(data_g[2, 3, ]$grid_data, array(0L, dim = c(1, 1, 1)))
  expect_identical(data_g[3, 2, ]$grid_data, array(NA_integer_, dim = c(1, 1, 1)))
})

test_that("subset assign works for grd_data_generic (identity)", {
  data <- array(0:5, dim = c(2, 3, 1))
  data_g <- grd_data_generic(data, data_order = c("y", "x", NA))

  data2 <- data
  data_g2 <- data_g
  data2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  data_g2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  expect_identical(data_g2$grid_data, data2)

  data2 <- data
  data_g2 <- data_g
  data2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  data_g2[1:2, 1:2, ] <- grd_data_generic(matrix(0:3, nrow = 2, ncol = 2))
  expect_identical(data_g2$grid_data, data2)

  data2 <- data
  data_g2 <- data_g
  data2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  data_g2[1:2, 1:2, ] <- grd_data_generic(array(0:3, dim = c(2, 2, 1)))
  expect_identical(data_g2$grid_data, data2)

  data2 <- data
  data_g2 <- data_g
  data2[, 1, ] <- 100L
  data_g2[, 1, ] <- 100L
  expect_identical(data_g2$grid_data, data2)

  data2 <- data
  data_g2 <- data_g
  data2[1, , ] <- 100L
  data_g2[1, , ] <- 100L
  expect_identical(data_g2$grid_data, data2)
})

test_that("subset assign works for grd_data_generic (row-major)", {
  data <- array(0:5, dim = c(2, 3, 1))
  data_g <- grd_data_generic(data, data_order = c("x", "y", NA))

  data_g2 <- data_g
  data_g2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  expect_identical(
    data_g2$grid_data,
    array(c(0L, 2L, 1L, 3L, 4L, 5L), dim = c(2, 3, 1))
  )

  data_g2 <- data_g
  data_g2[1:2, 1:2, ] <- grd_data_generic(
    array(0:3, dim = c(2, 2, 1)),
    data_order = c("x", "y", NA)
  )
  expect_identical(data_g2, data_g)
})

test_that("subset assign works for grd_data_generic (flipped)", {
  data <- array(0:5, dim = c(2, 3, 1))
  data_g <- grd_data_generic(data, data_order = c("-y", "-x", NA))

  data_g2 <- data_g
  data_g2[1:2, 1:2, ] <- matrix(0:3, nrow = 2, ncol = 2)
  expect_identical(
    grd_data_collect(data_g2),
    array(c(0L, 1L, 2L, 3L, 1L, 0L), dim = c(2, 3, 1))
  )

  data_g2 <- data_g
  data_g2[1:2, 1:2, ] <- grd_data_generic(
    array(0:3, dim = c(2, 2, 1)),
    data_order = c("-y", "-x", NA)
  )
  expect_identical(
    grd_data_collect(data_g2),
    array(c(3L, 2L, 1L, 0L, 1L, 0L), dim = c(2, 3, 1))
  )
})
