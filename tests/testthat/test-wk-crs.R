
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
  expect_identical(wk_crs_output(NULL, NULL), NULL)
  expect_identical(wk_crs_output(NULL, wk_crs_inherit()), NULL)
  expect_identical(wk_crs_output(wk_crs_inherit(), NULL), NULL)
  expect_identical(wk_crs_output(wk_crs_inherit(), wk_crs_inherit()), wk_crs_inherit())
  expect_error(wk_crs_output(1, 2), "Can't compare CRS objects")
})
