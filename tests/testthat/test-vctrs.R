
test_that("vctrs wkb implementation works", {
  expect_true(vctrs::vec_is(wkb()))
  expect_identical(vctrs::vec_size(wkb()), 0L)
  expect_identical(vctrs::vec_cast(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(wkt(), wkb()), wkb())
  expect_identical(vctrs::vec_proxy(wkb()), list())
  expect_identical(vctrs::vec_restore(list(), wkb()), wkb())
})
