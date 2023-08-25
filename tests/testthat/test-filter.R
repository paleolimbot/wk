
test_that("wk_identity() works", {
  diverse_wkt <- wkt(
    c(
      NA, "POINT EMPTY", "POINT (1 2)",
      "POLYGON ((0 0, 0 1, 1 0, 0 0))"
    )
  )
  expect_identical(wk_identity(diverse_wkt), diverse_wkt)

  expect_error(wk_identity(new_wk_wkt("NOT WKT")), "Expected")
})

test_that("wk_identity() propagates attributes", {
  expect_identical(
    wk_identity(
      wkt("LINESTRING ZM (0 0 0 0, 1 0 0 0)", crs = 1234, geodesic = TRUE)
    ),
    wkt("LINESTRING ZM (0 0 0 0, 1 0 0 0)", crs = 1234, geodesic = TRUE)
  )
})
