
test_that("wkb_coords() works", {
  # point
  wkb <- wkt_translate_wkb("POINT (30 10)")
  expect_identical(
    wkb_coords(wkb),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      ring_id = 0L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )
})

test_that("wksxp_coords() works", {
  # point
  sxp <- wkt_translate_wksxp("POINT (30 10)")
  expect_identical(
    wksxp_coords(sxp),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      ring_id = 0L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )
})

test_that("wkt_coords() works", {
  # point
  expect_identical(
    wkt_coords("POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      ring_id = 0L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )

  # point zm
  expect_identical(
    wkt_coords("POINT ZM (30 10 1 2)"),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      ring_id = 0L,
      x = 30,
      y = 10,
      z = 1,
      m = 2
    )
  )

  # linestring
  expect_identical(
    wkt_coords("LINESTRING (30 10, 20 11)"),
    data.frame(
      feature_id = c(1L, 1L),
      part_id = c(1L, 1L),
      ring_id = c(0L, 0L),
      x = c(30, 20),
      y = c(10, 11),
      z = c(NA_real_, NA_real_),
      m = c(NA_real_, NA_real_)
    )
  )

  # polygon
  expect_identical(
    wkt_coords("POLYGON ((30 10, 20 11, 0 0, 30 10))"),
    data.frame(
      feature_id = c(1L, 1L, 1L, 1L),
      part_id = c(1L, 1L, 1L, 1L),
      ring_id = c(1L, 1L, 1L, 1L),
      x = c(30, 20, 0, 30),
      y = c(10, 11, 0, 10),
      z = c(NA_real_, NA_real_, NA_real_, NA_real_),
      m = c(NA_real_, NA_real_, NA_real_, NA_real_)
    )
  )

  # multipoint
  expect_identical(
    wkt_coords("MULTIPOINT ((30 10), (20 11))"),
    data.frame(
      feature_id = c(1L, 1L),
      part_id = c(2L, 3L),
      ring_id = c(0L, 0L),
      x = c(30, 20),
      y = c(10, 11),
      z = c(NA_real_, NA_real_),
      m = c(NA_real_, NA_real_)
    )
  )

  # collection
  # point
  expect_identical(
    wkt_coords("GEOMETRYCOLLECTION (POINT (30 10))"),
    data.frame(
      feature_id = 1L,
      part_id = 2L,
      ring_id = 0L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )
})

test_that("sep_na works as intended", {
  holes <- c(
    "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0), (2 2, 2 4, 4 4, 4 2, 2 2))",
    "POLYGON ((11 11, 20 11, 20 20, 11 20, 11 11), (12 12, 12 14, 14 14, 14 12, 12 12))"
  )

  expect_identical(
    wkt_coords(holes, sep_na = FALSE)$ring_id,
    c(rep(1L, 5), rep(2L, 5), rep(3L, 5), rep(4L, 5))
  )

  expect_identical(
    wkt_coords(holes, sep_na = TRUE)$ring_id,
    c(rep(1L, 5), NA, rep(2L, 5), NA, rep(3L, 5), NA, rep(4L, 5))
  )

  # multi-geometries should only separate between simple geoms
  expect_identical(
    wkt_coords("MULTIPOINT ((30 10), (0 0))", sep_na = TRUE)$part_id,
    c(2L, NA, 3L)
  )

  # null geoms at the start shouldn't insert a separator
  expect_identical(
    wkt_coords(c(NA, "POINT (30 10)"), sep_na = TRUE)$feature_id,
    2L
  )

  # empty geoms at the start shouldn't insert a separator
  expect_identical(
    wkt_coords(c("POINT EMPTY", "POINT (30 10)"), sep_na = TRUE)$feature_id,
    2L
  )
})
