
test_that("basic translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wkt("POINT (30 10)"),
    "POINT (30 10)"
  )
  expect_identical(
    wkt_translate_wkt("LINESTRING (30 10, 0 0)"),
    "LINESTRING (30 10, 0 0)"
  )
  expect_identical(
    wkt_translate_wkt("POLYGON ((30 10, 0 0, 10 10, 30 10))"),
    "POLYGON ((30 10, 0 0, 10 10, 30 10))"
  )

})
