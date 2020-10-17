
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
