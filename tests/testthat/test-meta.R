
test_that("wk_meta() works", {
  wkt_empty <- c(
    "POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
    "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
    "GEOMETRYCOLLECTION EMPTY"
  )

  expect_identical(wk_meta(as_wkt(wkt_empty))$geometry_type, 1:7)
  expect_identical(wk_meta(as_wkb(wkt_empty))$geometry_type, 1:7)
  expect_true(all(wk_meta(as_wkt(wkt_empty))$size == 0))

  expect_identical(
    wk_meta(as_wkt(c("POINT (1 2)", NA)))$geometry_type,
    c(1L, NA)
  )

  expect_identical(
    wk_meta(as_wkt(c("POINT (1 2)", "POINT Z (1 2 3)")))$has_z,
    c(FALSE, TRUE)
  )

  expect_identical(
    wk_meta(as_wkt(c("POINT (1 2)", "POINT M (1 2 3)")))$has_m,
    c(FALSE, TRUE)
  )

  expect_identical(
    wk_meta(as_wkt(c("POINT (1 2)", "SRID=12;POINT (1 2)")))$srid,
    c(NA_integer_, 12L)
  )
})
