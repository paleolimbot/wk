
test_that("wkb class works", {
  x <- wkb(wkt_translate_wkb(c("POINT (40 10)")))
  expect_is(x, "wk_wkb")
  expect_is(x, "wk_vctr")
  expect_output(print(x), "wk_wkb")
  expect_identical(as_wkb(x), x)
  expect_is(wkb(list(NULL)), "wk_wkb")

  expect_error(new_wk_wkb(structure(list(), thing = "stuff")), "must be a list")
  expect_error(new_wk_wkb("char!"), "must be a list")
  expect_error(wkb(list(raw())), "Encountered 1 parse problem")
  expect_error(wkb(rep(list(raw()), 10)), "Encountered 10 parse problem")

  expect_is(x[1], "wk_wkb")
  expect_identical(x[[1]], x[1])
  expect_is(c(x, x), "wk_wkb")
  expect_identical(rep(x, 2), c(x, x))
  expect_identical(rep_len(x, 2), c(x, x))
  expect_length(c(x, x), 2)
})
