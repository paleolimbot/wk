
test_that("wkb_meta() works", {
  expect_identical(
    wkb_meta(wkt_translate_wkb("POINT (30 10)")),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )
})

test_that("wkt_meta() works", {
  expect_identical(
    wkt_meta("POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )
})

test_that("wkb_meta() works", {
  expect_identical(
    wksxp_meta(wkt_translate_wksxp("POINT (30 10)")),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 1L,
      size = 1L,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )
})

test_that("wkt_streamer_meta() works", {
  # point
  expect_identical(
    wkt_streamer_meta("POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 1L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )

  # multipoint
  expect_identical(
    wkt_streamer_meta("MULTIPOINT ((30 10))", recursive = FALSE),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 4L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )

  # multipoint recursive
  expect_identical(
    wkt_streamer_meta("MULTIPOINT ((30 10))", recursive = TRUE),
    data.frame(
      feature_id = c(1L, 1L),
      nest_id = c(0L, 0L),
      part_id = c(NA_integer_, 1L),
      type_id = c(4L, 1L),
      size = c(NA_integer_, NA_integer_),
      srid = c(NA_integer_, NA_integer_),
      has_z = c(FALSE, FALSE),
      has_m = c(FALSE, FALSE)
    )
  )

  # collection
  expect_identical(
    wkt_streamer_meta("GEOMETRYCOLLECTION (POINT (30 10))", recursive = FALSE),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 7L,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = FALSE,
      has_m = FALSE
    )
  )

  # collection recursive
  expect_identical(
    wkt_streamer_meta("GEOMETRYCOLLECTION (POINT (30 10))", recursive = TRUE),
    data.frame(
      feature_id = c(1L, 1L),
      nest_id = c(0L, 1L),
      part_id = c(NA_integer_, 1L),
      type_id = c(7L, 1L),
      size = c(NA_integer_, NA_integer_),
      srid = c(NA_integer_, NA_integer_),
      has_z = c(FALSE, FALSE),
      has_m = c(FALSE, FALSE)
    )
  )
})

test_that("wkt_streamer_meta() works with NULLs", {
  expect_identical(
    wkt_streamer_meta(NA),
    data.frame(
      feature_id = 1L,
      nest_id = NA_integer_,
      part_id = NA_integer_,
      type_id = NA_integer_,
      size = NA_integer_,
      srid = NA_integer_,
      has_z = NA,
      has_m = NA
    )
  )
})

test_that("wkt_streamer_meta() returns SRIDs if present", {
  expect_identical(
    wkt_streamer_meta("SRID=33;POINT (30 10)"),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      type_id = 1L,
      size = NA_integer_,
      srid = 33L,
      has_z = FALSE,
      has_m = FALSE
    )
  )
})

test_that("wkt_streamer_meta() fails on parse error", {
  expect_error(wkt_streamer_meta("NOPE"), class = "WKParseException")
})
