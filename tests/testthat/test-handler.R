
test_that("void and debug handlers can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
  expect_is(wk_debug_handler(), "wk_debug_handler")
  expect_is(wk_debug_handler(), "wk_handler")
})

test_that("wk_handlers have a default print method", {
  expect_output(print(wk_void_handler()), "wk_void_handler")
})
