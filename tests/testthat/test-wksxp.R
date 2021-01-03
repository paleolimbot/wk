
test_that("wksxp class works", {
  x <- wksxp(wkt_translate_wksxp("POINT (40 10)"))
  expect_is(x, "wk_wksxp")
  expect_is(x, "wk_vctr")
  expect_true(is_wk_wksxp(x))
  expect_output(print(x), "wk_wksxp")

  expect_is(wksxp(list(NULL)), "wk_wksxp")
  expect_true(is.na(wksxp(list(NULL))))

  expect_error(new_wk_wksxp(structure(list(), thing = "stuff")), "must be a list")
  expect_error(new_wk_wksxp("char!"), "must be a list")

  expect_is(x[1], "wk_wksxp")
  expect_identical(x[[1]], x[1])
  expect_is(c(x, x), "wk_wksxp")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep_len(x, 2), c(x, x))
  expect_identical(rep(wksxp(), 5), wksxp())
  expect_length(c(x, x), 2)
})
