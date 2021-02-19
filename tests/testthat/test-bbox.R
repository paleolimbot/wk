
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
