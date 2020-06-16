
test_that("wkb_meta() works", {
  expect_identical(
    wkb_meta(wkt_translate_wkb("POINT (30 10)")),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = 1L
    )
  )
})

test_that("wkt_meta() works", {
  expect_identical(
    wkt_meta("POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = 1L
    )
  )
})

test_that("wkb_meta() works", {
  expect_identical(
    wksxp_meta(wkt_translate_wksxp("POINT (30 10)")),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = 1L
    )
  )
})

test_that("wkt_streamer_meta() works", {
  # point
  expect_identical(
    wkt_streamer_meta("POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 1L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = NA_integer_
    )
  )

  # multipoint
  expect_identical(
    wkt_streamer_meta("MULTIPOINT ((30 10))", recursive = FALSE),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 4L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = NA_integer_
    )
  )

  # multipoint recursive
  expect_identical(
    wkt_streamer_meta("MULTIPOINT ((30 10))", recursive = TRUE),
    data.frame(
      feature_id = c(1L, 1L),
      part_id = c(1L, 2L),
      type_id = c(4L, 1L),
      size = c(NA_integer_, NA_integer_),
      srid = c(NA_integer_, NA_integer_),
      has_z = c(FALSE, FALSE),
      has_m = c(FALSE, FALSE),
      n_coords = c(0L, 1L)
    )
  )

  # collection
  expect_identical(
    wkt_streamer_meta("GEOMETRYCOLLECTION (POINT (30 10))", recursive = FALSE),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 7L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = NA_integer_
    )
  )

  # collection recursive
  expect_identical(
    wkt_streamer_meta(c("GEOMETRYCOLLECTION (POINT (30 10))", NA), recursive = TRUE),
    data.frame(
      feature_id = c(1L, 1L, 2L),
      part_id = c(1L, 2L, NA_integer_),
      type_id = c(7L, 1L, NA_integer_),
      size = c(NA_integer_, NA_integer_, NA_integer_),
      srid = c(NA_integer_, NA_integer_, NA_integer_),
      has_z = c(FALSE, FALSE, NA),
      has_m = c(FALSE, FALSE, NA),
      n_coords = c(0L, 1L, NA_integer_)
    )
  )
})

test_that("wkt_streamer_meta() works with NULLs", {
  expect_identical(
    wkt_streamer_meta(NA),
    data.frame(
      feature_id = 1L,
      part_id = NA_integer_,
      type_id = NA_integer_,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = NA,
      has_m = NA,
      n_coords = NA_integer_
    )
  )
})

test_that("wkt_meta() counts coordinates when NULLs are present", {
  expect_identical(
    wkt_meta(c("LINESTRING (20 20, 0 0)", NA)),
    data.frame(
      feature_id = c(1L, 2L),
      part_id = c(1L, NA_integer_),
      type_id = c(2L, NA_integer_),
      size = c(2L, NA_integer_),
      srid = c(NA_integer_, NA_integer_),
      has_z = c(FALSE, NA),
      has_m = c(FALSE, NA),
      n_coords = c(2L, NA_integer_)
    )
  )
})

test_that("wkt_streamer_meta() returns SRIDs if present", {
  expect_identical(
    wkt_streamer_meta("SRID=33;POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      part_id = 1L,
      type_id = 1L,
      size = NA_integer_,
      srid = 33L,
      has_z = FALSE,
      has_m = FALSE,
      n_coords = NA_integer_
    )
  )
})

test_that("wkt_streamer_meta() fails on parse error", {
  expect_error(wkt_streamer_meta("NOPE"), class = "WKParseException")
})

test_that("geometry type converters work", {
  types_str <- c(
    "point", "linestring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )
  expect_identical(wk_geometry_type_id(types_str), 1:7)
  expect_identical(wk_geometry_type(7:1), rev(types_str))
})
