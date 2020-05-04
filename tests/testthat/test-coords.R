
test_that("wkb_coords() works", {
  # point
  wkb <- wkt_translate_wkb("POINT (30 10)")
  expect_identical(
    wkb_coords(wkb),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      ring_id = NA_integer_,
      coord_id = 1L,
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
      nest_id = 0L,
      part_id = NA_integer_,
      ring_id = NA_integer_,
      coord_id = 1L,
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
      nest_id = 0L,
      part_id = NA_integer_,
      ring_id = NA_integer_,
      coord_id = 1L,
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
      nest_id = c(0L, 0L),
      part_id = c(NA_integer_, NA_integer_),
      ring_id = c(NA_integer_, NA_integer_),
      coord_id = c(1L, 2L),
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
      nest_id = c(0L, 0L, 0L, 0L),
      part_id = c(NA_integer_, NA_integer_, NA_integer_, NA_integer_),
      ring_id = c(1L, 1L, 1L, 1L),
      coord_id = c(1L, 2L, 3L, 4L),
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
      nest_id = c(0L, 0L),
      part_id = c(1L, 2L),
      ring_id = c(NA_integer_, NA_integer_),
      coord_id = c(1L, 1L),
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
      nest_id = 1L,
      part_id = 1L,
      ring_id = NA_integer_,
      coord_id = 1L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )
})
