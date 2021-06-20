
test_that("wk_linestring() works", {
  expect_identical(wk_linestring(wkt()), wkt("LINESTRING EMPTY", crs = wk_crs_inherit()))
  expect_identical(
    wk_linestring(wkt(NA_character_)),
    wkt("LINESTRING EMPTY")
  )
  expect_identical(
    wk_linestring(wkt("POINT EMPTY")),
    wkt("LINESTRING EMPTY")
  )

  expect_identical(
    wk_linestring(xy(1:4, 1), feature_id = 1L),
    as_wkb("LINESTRING (1 1, 2 1, 3 1, 4 1)")
  )
  expect_identical(
    wk_linestring(xy(1:4, 1), feature_id = c(1L, 1L, 2L, 2L)),
    as_wkb(c("LINESTRING (1 1, 2 1)", "LINESTRING (3 1, 4 1)"))
  )
})
