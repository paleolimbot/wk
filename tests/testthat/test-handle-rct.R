
test_that("wk_handle.wk_rct() works", {
  expect_identical(
    wk_handle(rct(c(1, NA, Inf, 0), c(2, NA, 0, Inf), c(3, NA, 1, 1), c(4, NA, 1, 1)), wkt_writer()),
    wkt(c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", "POLYGON EMPTY", "POLYGON EMPTY", "POLYGON EMPTY"))
  )

  # check invalid data
  expect_error(wk_handle.wk_rct("not a rct", wk_void_handler()), "does not inherit from")
})
