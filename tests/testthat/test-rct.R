
test_that("rct class works", {
  expect_s3_class(rct(), "wk_rct")
  expect_output(print(rct(1, 2, 3, 4)), "\\[1 2 3 4\\]")
  expect_identical(as_rct(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))

  expect_identical(
    as_rct(as.matrix(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4))),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4)),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(matrix(1:4, nrow = 1)),
    rct(1, 2, 3, 4)
  )
})

test_that("coercion to and from wk* classes works", {
  expect_identical(
    as_wkt(rct(1, 2, 3, 4)),
    wkt("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )

  expect_identical(
    as_wkb(rct(1, 2, 3, 4)),
    as_wkb("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )
})

test_that("subset-assign works for rct", {
  x <- rct(1:2, 2:3, 3:4, 4:5)
  x[1] <- rct(NA, NA, NA, NA)
  expect_identical(x, c(rct(NA, NA, NA, NA), rct(2, 3, 4, 5)))
})

test_that("rct() propagates CRS", {
  x <- rct(1, 2, 3, 4)
  wk_crs(x) <- 1234

  expect_identical(wk_crs(x[1]), 1234)
  expect_identical(wk_crs(c(x, x)), 1234)
  expect_identical(wk_crs(rep(x, 2)), 1234)

  expect_error(x[1] <- wk_set_crs(x, NULL), "are not equal")
  x[1] <- wk_set_crs(x, 1234L)
  expect_identical(wk_crs(x), 1234)
})

test_that("rct() accessors return the correct values", {
  x <- rct(0, 1, 2, 4)
  expect_identical(rct_xmin(x), 0)
  expect_identical(rct_xmax(x), 2)
  expect_identical(rct_ymin(x), 1)
  expect_identical(rct_ymax(x), 4)

  expect_identical(rct_width(x), 2)
  expect_identical(rct_height(x), 3)
})

test_that("rct_intersection() works", {
  expect_identical(
    rct_intersection(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    rct(xmin = 5, xmax = 10, ymin = 5, ymax = 10)
  )

  expect_identical(
    rct_intersection(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    rct(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_)
  )

  expect_identical(
    rct_intersection(
      rct(xmin = NA_real_, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    rct(xmin = NA_real_, xmax = NA_real_, ymin = NA_real_, ymax = NA_real_)
  )
})

test_that("rectangle intersector predicate works", {
  expect_identical(
    rct_intersects(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    TRUE
  )

  expect_identical(
    rct_intersects(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 15, xmax = 25, ymin = 15, ymax = 25)
    ),
    FALSE
  )

  expect_identical(
    rct_intersects(
      rct(xmin = NA_real_, xmax = 10, ymin = 0, ymax = 10),
      rct(xmin = 5, xmax = 15, ymin = 5, ymax = 15)
    ),
    NA
  )
})

test_that("rectangle contains predicate works", {
  expect_true(
    rct_contains(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      xy(x = 5, y = 2)
    )
  )

  expect_false(
    rct_contains(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      xy(x = 11, y = 2)
    )
  )

  expect_false(
    rct_contains(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      xy(x = 5, y = 11)
    )
  )

  expect_identical(
    rct_contains(
      rct(xmin = 0, xmax = 10, ymin = 0, ymax = 10),
      xy(x = NA_real_, y = 2)
    ),
    NA
  )
})
