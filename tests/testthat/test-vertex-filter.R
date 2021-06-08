
test_that("wk_vertices() works", {
  expect_identical(
    wk_vertices(wkt(c("POINT (0 0)", "POINT (1 1)"))),
    wkt(c("POINT (0 0)", "POINT (1 1)"))
  )
  expect_identical(
    wk_vertices(wkt("LINESTRING (0 0, 1 1)")),
    wkt(c("POINT (0 0)", "POINT (1 1)"))
  )
})
