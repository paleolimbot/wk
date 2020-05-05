
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


