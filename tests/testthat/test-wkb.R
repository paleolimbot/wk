
test_that("wkb class works", {
  x <- wkb(wkt_translate_wkb(c("POINT (40 10)")))
  expect_is(x, "wk_wkb")
  expect_is(x, "wk_vctr")
  expect_output(print(x), "wk_wkb")
  expect_identical(as_wkb(x), x)

  expect_is(x[1], "wk_wkb")
  expect_identical(x[[1]], x[1])
  expect_is(c(x, x), "wk_wkb")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep_len(x, 2), c(x, x))
  expect_length(c(x, x), 2)
})
