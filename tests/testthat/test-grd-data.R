
test_that("grd_data() works", {
  d <- matrix()
  expect_identical(grd_data(grd_rct(d)), d)
})

test_that("nativeRaster backed grd objects have correct data_order", {
  col_native <- structure(
    c(-16777216L, -13421773L, -10066330L, -15066598L, -11711155L, -8355712L),
    .Dim = 2:3,
    class = "nativeRaster"
  )
  expect_identical(grd_data_order(col_native), c("x", "y"))
})
