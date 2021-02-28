
test_that("wk_count() works", {
  expect_identical(
    wk_count(wkt(c("POINT (1 2)", "POLYGON ((0 0, 0 1, 1 1, 0 0))", NA))),
    data.frame(
      n_geom = c(1L, 1L, 0L),
      n_ring = c(0L, 1L, 0L),
      n_coord = c(1, 4L, 0)
    )
  )
})
