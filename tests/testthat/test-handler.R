
test_that("wk_handler class works", {
  expect_true(is_wk_handler(wk_void_handler()))
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_output(print(wk_void_handler()), "wk_void_handler")
  expect_s3_class(as_wk_handler(wk_void_handler), "wk_void_handler")
})

test_that("is_handleable works", {
  expect_true(is_handleable(xy()))
  expect_false(is_handleable(1:5))
})

test_that("as_handler() works", {
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_identical(as_wk_handler(function() handler), handler)
  expect_error(as_wk_handler(3), "must be a wk handler")
})
