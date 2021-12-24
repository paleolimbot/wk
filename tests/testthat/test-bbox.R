
test_that("wk_bbox() works", {
  expect_identical(
    wk_bbox(wkt("LINESTRING (1 2, 3 4)")),
    rct(1, 2, 3, 4)
  )

  expect_identical(
    wk_bbox(wkt(crs = NULL)),
    rct(Inf, Inf, -Inf, -Inf)
  )

  expect_identical(
    wk_bbox(wkt(crs = 1234)),
    rct(Inf, Inf, -Inf, -Inf, crs = 1234)
  )
})

test_that("wk_bbox() works when vector has cached bbox", {
  skip_if_not_installed("sf")

  sf_linestring <- sf::st_sfc(sf::st_linestring(rbind(c(1, 2), c(3, 4))))
  expect_identical(
    wk_bbox(sf_linestring),
    rct(1, 2, 3, 4, crs = sf::NA_crs_)
  )
})

test_that("wk_bbox() works when geometry has cached bbox", {
  expect_identical(wk_bbox(xy(1:3, 2:4)), rct(1, 2, 3, 4))
  expect_identical(wk_bbox(crc(2, 3, r = 1)), rct(1, 2, 3, 4))
  expect_identical(wk_bbox(rct(1, 2, 3, 4)), rct(1, 2, 3, 4))
})

test_that("wk_envelope() works", {
  expect_identical(
    wk_envelope(wkt("LINESTRING (1 2, 3 4)")),
    rct(1, 2, 3, 4)
  )

  expect_identical(
    wk_envelope(wkt(NA_character_)),
    rct(NA_real_, NA_real_, NA_real_, NA_real_)
  )

  expect_identical(
    wk_envelope(wkt(crs = NULL)),
    rct(crs = NULL)
  )

  expect_identical(
    wk_envelope(wkt(crs = 1234)),
    rct(crs = 1234)
  )
})

test_that("wk_envelope() works when geometry has cached bbox", {
  expect_identical(
    wk_handle(crc(2, 3, r = 1), wk_envelope_handler()),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    wk_handle(rct(1, 2, 3, 4), wk_envelope_handler()),
    rct(1, 2, 3, 4)
  )
})

test_that("wk_envelope() works when vector size is unknown", {
  expect_identical(
    handle_wkt_without_vector_size(
      wkt("POINT (0 1)"),
      wk_envelope_handler()
    ),
    rct(0, 1, 0, 1)
  )

  expect_identical(
    handle_wkt_without_vector_size(
      wkt(rep("POINT (0 1)", 1025)),
      wk_envelope_handler()
    ),
    rep(rct(0, 1, 0, 1), 1025)
  )
})

test_that("wk_envelope() optimization works for xy()", {
  expect_identical(
    wk_envelope(xy(1, 2, crs = 1234)),
    rct(1, 2, 1, 2, crs = 1234)
  )
})

test_that("wk_envelope() optimization works for rct()", {
  expect_identical(
    wk_envelope(rct(1, 2, 3, 4, crs = 1234)),
    rct(1, 2, 3, 4, crs = 1234)
  )
})

test_that("wk_envelope() optimization works for crc()", {
  expect_identical(
    wk_envelope(crc(2, 3, r = 1, crs = 1234)),
    rct(1, 2, 3, 4, crs = 1234)
  )
})

test_that("wk_bbox() and wk_envelope() fail for geodesic objects", {
  expect_error(
    wk_bbox(wkt("LINESTRING (0 1)", geodesic = TRUE)),
    "Can't compute bbox for geodesic object"
  )

  expect_error(
    wk_envelope(wkt("LINESTRING (0 1)", geodesic = TRUE)),
    "Can't compute envelope for geodesic object"
  )

  expect_error(
    wk_envelope(rct(geodesic = TRUE)),
    "Can't compute envelope for geodesic object"
  )
})
