
test_that("wk_identity() works", {
  expect_identical(wk_identity(wkt("POINT (1 2)")), wkt("POINT (1 2)"))
})
