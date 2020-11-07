
test_that("wk classes are vctrs", {
  expect_true(vctrs::vec_is(wkt()))
  expect_true(vctrs::vec_is(wkb()))
  expect_true(vctrs::vec_is(wksxp()))
  expect_true(vctrs::vec_is(xy()))
  expect_true(vctrs::vec_is(xyz()))
  expect_true(vctrs::vec_is(xym()))
  expect_true(vctrs::vec_is(xyzm()))
  expect_true(vctrs::vec_is(rct()))
})

test_that("wk classes can be proxied and restored", {
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(wkt()), wkt()), wkt())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(wkb()), wkb()), wkb())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(wksxp()), wksxp()), wksxp())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xy()), xy()), xy())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xyz()), xyz()), xyz())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xym()), xym()), xym())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xyzm()), xyzm()), xyzm())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(rct()), rct()), rct())
})

test_that("vctrs wkb implementation works", {
  expect_true(vctrs::vec_is(wkb()))
  expect_identical(vctrs::vec_size(wkb()), 0L)
  expect_identical(vctrs::vec_cast(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(wkt(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(wksxp(), wkb()), wkb())
  expect_identical(vctrs::vec_proxy(wkb(crs = NULL)), list())
  expect_identical(vctrs::vec_restore(list(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkb(), wksxp()), wksxp())
})

test_that("vctrs wkt implementation works", {
  expect_true(vctrs::vec_is(wkt()))
  expect_identical(vctrs::vec_size(wkt()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(wkb(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(wksxp(), wkt()), wkt())
  expect_identical(vctrs::vec_proxy(wkt(crs = NULL)), character())
  expect_identical(vctrs::vec_restore(character(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkb()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wksxp()), wksxp())
})

test_that("vctrs wksxp implementation works", {
  expect_true(vctrs::vec_is(wksxp()))
  expect_identical(vctrs::vec_size(wksxp()), 0L)
  expect_identical(vctrs::vec_cast(wksxp(), wksxp()), wksxp())
  expect_identical(vctrs::vec_cast(wkb(), wksxp()), wksxp())
  expect_identical(vctrs::vec_cast(wkt(), wksxp()), wksxp())
  expect_identical(vctrs::vec_proxy(wksxp(crs = NULL)), list())
  expect_identical(vctrs::vec_restore(list(), wksxp()), wksxp())
  expect_identical(vctrs::vec_c(wksxp(), wksxp()), wksxp())
  expect_identical(vctrs::vec_c(wksxp(), wkt()), wksxp())
  expect_identical(vctrs::vec_c(wksxp(), wkb()), wksxp())
})

test_that("vec_c() propagates the crs attribute", {
  expect_identical(
    vctrs::vec_c(wkt(crs = 1234), wkt(crs = 1234)),
    wkt(crs = 1234)
  )

  expect_identical(
    vctrs::vec_c(wkb(crs = 1234), wkb(crs = 1234)),
    wkb(crs = 1234)
  )

  expect_identical(
    vctrs::vec_c(wksxp(crs = 1234), wksxp(crs = 1234)),
    wksxp(crs = 1234)
  )
})
