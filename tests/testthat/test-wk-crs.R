
test_that("crs setting and getting works on wk_vctr",  {
  x <- new_wk_wkt()
  expect_null(wk_crs(x))

  x <- wk_set_crs(x, 4326)
  expect_identical(wk_crs(x), 4326)

  wk_crs(x) <- 26920
  expect_identical(wk_crs(x), 26920)
})

test_that("crs setting and getting works on wk_rcrd",  {
  x <- new_wk_xy()
  expect_null(wk_crs(x))

  x <- wk_set_crs(x, 4326)
  expect_identical(wk_crs(x), 4326)

  wk_crs(x) <- 26920
  expect_identical(wk_crs(x), 26920)
})

test_that("crs comparison works", {
  expect_true(wk_crs_equal(NULL, NULL))
  expect_false(wk_crs_equal(NULL, "something"))
  expect_false(wk_crs_equal("something", NULL))

  expect_true(wk_crs_equal("something", "something"))
  expect_false(wk_crs_equal("something", "something_else"))

  expect_true(wk_crs_equal(1234, 1234L))
  expect_true(wk_crs_equal(1234L, 1234))

  expect_false(wk_crs_equal(NULL, 1234))
})

test_that("crs output computing works", {
  x <- wkt("POINT (0 0)", crs = NULL)

  expect_identical(wk_crs_output(x, x), NULL)
  expect_identical(wk_crs_output(x, wk_set_crs(x, wk_crs_inherit())), NULL)
  expect_identical(wk_crs_output(wk_set_crs(x, wk_crs_inherit()), x), NULL)
  expect_identical(
    wk_crs_output(wk_set_crs(x, wk_crs_inherit()), wk_set_crs(x, wk_crs_inherit())),
    wk_crs_inherit()
  )
  expect_identical(wk_crs_output(wk_set_crs(x, 1), wkt()), 1)
  expect_identical(wk_crs_output(wkt(), wk_set_crs(x, 1)), 1)
  expect_error(wk_crs_output(wk_set_crs(x, 1), wk_set_crs(x, 2)), "are not equal")
})

test_that("wk_crs_inherit() prints as expected", {
  expect_match(format(wk_crs_inherit()), "wk_crs_inherit")
  expect_output(print(wk_crs_inherit()), "wk_crs_inherit")
})
