
test_that("wk_vctr class works", {
  x <- structure(1:5, class = "wk_vctr")
  expect_is(x, "wk_vctr")
  expect_is(x[1:2], "wk_vctr")
  expect_output(print(x), "wk_vctr")
  expect_output(print(stats::setNames(x, as.character(1:5))), "wk_vctr")
  expect_output(print(x[0]), "wk_vctr")
})
