
test_that("vctrs wkb implementation works", {
  expect_true(vctrs::vec_is(wkb()))
  expect_identical(vctrs::vec_size(wkb()), 0L)
  expect_identical(vctrs::vec_cast(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(wkt(), wkb()), wkb())
  expect_identical(vctrs::vec_proxy(wkb()), list())
  expect_identical(vctrs::vec_restore(list(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkt()), wkb())
})

test_that("vctrs wkt implementation works", {
  expect_true(vctrs::vec_is(wkt()))
  expect_identical(vctrs::vec_size(wkt()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(wkb(), wkt()), wkt())
  expect_identical(vctrs::vec_proxy(wkt()), character())
  expect_identical(vctrs::vec_restore(character(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkb()), wkt())
})

test_that("vctrs wksxp implementation works", {
  expect_true(vctrs::vec_is(wksxp()))
  expect_identical(vctrs::vec_size(wksxp()), 0L)
  expect_identical(vctrs::vec_cast(wksxp(), wksxp()), wksxp())
  expect_identical(vctrs::vec_cast(wkb(), wksxp()), wksxp())
  expect_identical(vctrs::vec_proxy(wksxp()), list())
  expect_identical(vctrs::vec_restore(list(), wksxp()), wksxp())
  expect_identical(vctrs::vec_c(wksxp(), wksxp()), wksxp())
  expect_identical(vctrs::vec_c(wksxp(), wkt()), wksxp())
})
