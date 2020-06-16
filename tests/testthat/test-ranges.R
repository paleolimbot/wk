
test_that("wkt_ranges() works", {
  expect_identical(
    wkt_ranges(c("POINT (1 2)", "POINT (5 6)")),
    data.frame(
      xmin = 1, ymin = 2, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = 6, zmax = -Inf, mmax = -Inf
    )
  )
  expect_identical(
    wkt_ranges(c("POINT Z (1 2 3)", "POINT (5 6)")),
    data.frame(
      xmin = 1, ymin = 2, zmin = 3, mmin = Inf,
      xmax = 5, ymax = 6, zmax = 3, mmax = -Inf
    )
  )
  expect_identical(
    wkt_ranges(c("POINT ZM (1 2 3 4)", "POINT (5 6)")),
    data.frame(
      xmin = 1, ymin = 2, zmin = 3, mmin = 4,
      xmax = 5, ymax = 6, zmax = 3, mmax = 4
    )
  )
  expect_identical(
    wkt_ranges(c("POINT (1 nan)", "POINT (5 6)")),
    data.frame(
      xmin = 1, ymin = NA_real_, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = NA_real_, zmax = -Inf, mmax = -Inf
    )
  )
  expect_identical(
    wkt_ranges(c("POINT (1 nan)", "POINT (5 6)"), na.rm = TRUE),
    data.frame(
      xmin = 1, ymin = 6, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = 6, zmax = -Inf, mmax = -Inf
    )
  )
  expect_identical(
    wkt_ranges(c("POINT (1 inf)", "POINT (5 6)")),
    data.frame(
      xmin = 1, ymin = 6, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = Inf, zmax = -Inf, mmax = -Inf
    )
  )
  expect_identical(
    wkt_ranges(c("POINT (1 inf)", "POINT (5 6)"), finite = TRUE),
    data.frame(
      xmin = 1, ymin = 6, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = 6, zmax = -Inf, mmax = -Inf
    )
  )

  expect_identical(
    wkt_ranges(c("POINT (nan nan)", "POINT (nan nan)"), finite = TRUE),
    wkt_ranges(character(0))
  )
  expect_identical(
    wkt_ranges(c("POINT (1 2)", "POINT (nan nan)"), finite = TRUE),
    wkt_ranges("POINT (1 2)")
  )
})

test_that("wkb_ranges() works", {
  expect_identical(
    wkb_ranges(wkt_translate_wkb(c("POINT (1 2)", "POINT (5 6)"))),
    data.frame(
      xmin = 1, ymin = 2, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = 6, zmax = -Inf, mmax = -Inf
    )
  )
})

test_that("wksxp_ranges() works", {
  expect_identical(
    wksxp_ranges(wkt_translate_wksxp(c("POINT (1 2)", "POINT (5 6)"))),
    data.frame(
      xmin = 1, ymin = 2, zmin = Inf, mmin = Inf,
      xmax = 5, ymax = 6, zmax = -Inf, mmax = -Inf
    )
  )
})

test_that("wkt_feature_ranges() works", {
  expect_identical(
    wkt_feature_ranges(c("POINT ZM (1 2 3 4)", "POINT ZM (5 6 7 8)")),
    data.frame(
      xmin = c(1, 5),
      ymin = c(2, 6),
      zmin = c(3, 7),
      mmin = c(4, 8),
      xmax = c(1, 5),
      ymax = c(2, 6),
      zmax = c(3, 7),
      mmax = c(4, 8)
    )
  )
})

test_that("wkb_feature_ranges() works", {
  expect_identical(
    wkb_feature_ranges(wkt_translate_wkb(c("POINT ZM (1 2 3 4)", "POINT ZM (5 6 7 8)"))),
    data.frame(
      xmin = c(1, 5),
      ymin = c(2, 6),
      zmin = c(3, 7),
      mmin = c(4, 8),
      xmax = c(1, 5),
      ymax = c(2, 6),
      zmax = c(3, 7),
      mmax = c(4, 8)
    )
  )
})

test_that("wkb_feature_ranges() works", {
  expect_identical(
    wksxp_feature_ranges(wkt_translate_wksxp(c("POINT ZM (1 2 3 4)", "POINT ZM (5 6 7 8)"))),
    data.frame(
      xmin = c(1, 5),
      ymin = c(2, 6),
      zmin = c(3, 7),
      mmin = c(4, 8),
      xmax = c(1, 5),
      ymax = c(2, 6),
      zmax = c(3, 7),
      mmax = c(4, 8)
    )
  )
})
