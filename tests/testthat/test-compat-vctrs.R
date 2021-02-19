
test_that("wk classes are vctrs", {
  expect_true(vctrs::vec_is(wkt()))
  expect_true(vctrs::vec_is(wkb()))
  expect_true(vctrs::vec_is(xy()))
  expect_true(vctrs::vec_is(xyz()))
  expect_true(vctrs::vec_is(xym()))
  expect_true(vctrs::vec_is(xyzm()))
  expect_true(vctrs::vec_is(rct()))
})

test_that("wk classes can be proxied and restored", {
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(wkt()), wkt()), wkt())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(wkb()), wkb()), wkb())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xy()), xy()), xy())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xyz()), xyz()), xyz())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xym()), xym()), xym())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(xyzm()), xyzm()), xyzm())
  expect_identical(vctrs::vec_restore(vctrs::vec_proxy(rct()), rct()), rct())
})

test_that("vctrs wkb implementation works", {
  expect_true(vctrs::vec_is(wkb()))
  expect_identical(vctrs::vec_size(wkb()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(xy(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(xyz(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(xym(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(xyzm(), wkb()), wkb())
  expect_identical(vctrs::vec_cast(rct(), wkb()), wkb())
  expect_identical(vctrs::vec_proxy(wkb(crs = NULL)), list())
  expect_identical(vctrs::vec_restore(list(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkb()), wkb())
  expect_identical(vctrs::vec_c(wkb(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkb(), xy()), wkb())
  expect_identical(vctrs::vec_c(wkb(), xyz()), wkb())
  expect_identical(vctrs::vec_c(wkb(), xym()), wkb())
  expect_identical(vctrs::vec_c(wkb(), xyzm()), wkb())
  expect_identical(vctrs::vec_c(wkb(), rct()), wkb())
})

test_that("vctrs wkt implementation works", {
  expect_true(vctrs::vec_is(wkt()))
  expect_identical(vctrs::vec_size(wkt()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(wkb(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(xy(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(xyz(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(xym(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(xyzm(), wkt()), wkt())
  expect_identical(vctrs::vec_cast(rct(), wkt()), wkt())
  expect_identical(vctrs::vec_proxy(wkt(crs = NULL)), character())
  expect_identical(vctrs::vec_restore(character(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkt()), wkt())
  expect_identical(vctrs::vec_c(wkt(), wkb()), wkt())
  expect_identical(vctrs::vec_c(wkt(), xy()), wkt())
  expect_identical(vctrs::vec_c(wkt(), xyz()), wkt())
  expect_identical(vctrs::vec_c(wkt(), xym()), wkt())
  expect_identical(vctrs::vec_c(wkt(), xyzm()), wkt())
  expect_identical(vctrs::vec_c(wkt(), rct()), wkt())
})

test_that("vctrs xy implementation works", {
  expect_true(vctrs::vec_is(xy()))
  expect_identical(vctrs::vec_size(xy()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), xy()), xy())
  expect_identical(vctrs::vec_cast(wkb(), xy()), xy())
  expect_identical(vctrs::vec_cast(xy(), xy()), xy())
  expect_identical(vctrs::vec_cast(xyz(), xy()), xy())
  expect_identical(vctrs::vec_cast(xym(), xy()), xy())
  expect_identical(vctrs::vec_cast(xyzm(), xy()), xy())
  expect_error(vctrs::vec_cast(rct(), xy()), class = "vctrs_error_incompatible_type")
  expect_identical(vctrs::vec_proxy(xy(crs = NULL)), data.frame(x = double(), y = double()))
  expect_identical(vctrs::vec_restore(data.frame(x = double(), y = double()), xy()), xy())
  expect_identical(vctrs::vec_c(xy(), wkt()), wkt())
  expect_identical(vctrs::vec_c(xy(), wkb()), wkb())
  expect_identical(vctrs::vec_c(xy(), xy()), xy())
  expect_identical(vctrs::vec_c(xy(), xyz()), xyz())
  expect_identical(vctrs::vec_c(xy(), xym()), xym())
  expect_identical(vctrs::vec_c(xy(), xyzm()), xyzm())
  expect_identical(vctrs::vec_c(xy(), rct()), wkb())
})

test_that("vctrs xyz implementation works", {
  expect_true(vctrs::vec_is(xyz()))
  expect_identical(vctrs::vec_size(xyz()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), xyz()), xyz())
  expect_identical(vctrs::vec_cast(wkb(), xyz()), xyz())
  expect_identical(vctrs::vec_cast(xy(), xyz()), xyz())
  expect_identical(vctrs::vec_cast(xyz(), xyz()), xyz())
  expect_identical(vctrs::vec_cast(xym(), xyz()), xyz())
  expect_identical(vctrs::vec_cast(xyzm(), xyz()), xyz())
  expect_error(vctrs::vec_cast(rct(), xyz()), class = "vctrs_error_incompatible_type")
  expect_identical(vctrs::vec_proxy(xyz(crs = NULL)), data.frame(x = double(), y = double(), z = double()))
  expect_identical(vctrs::vec_restore(data.frame(x = double(), y = double(), z = double()), xyz()), xyz())
  expect_identical(vctrs::vec_c(xyz(), wkt()), wkt())
  expect_identical(vctrs::vec_c(xyz(), wkb()), wkb())
  expect_identical(vctrs::vec_c(xyz(), xy()), xyz())
  expect_identical(vctrs::vec_c(xyz(), xyz()), xyz())
  expect_identical(vctrs::vec_c(xyz(), xym()), xyzm())
  expect_identical(vctrs::vec_c(xyz(), xyzm()), xyzm())
  expect_identical(vctrs::vec_c(xyz(), rct()), wkb())
})

test_that("vctrs xym implementation works", {
  expect_true(vctrs::vec_is(xym()))
  expect_identical(vctrs::vec_size(xym()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), xym()), xym())
  expect_identical(vctrs::vec_cast(wkb(), xym()), xym())
  expect_identical(vctrs::vec_cast(xy(), xym()), xym())
  expect_identical(vctrs::vec_cast(xyz(), xym()), xym())
  expect_identical(vctrs::vec_cast(xym(), xym()), xym())
  expect_identical(vctrs::vec_cast(xyzm(), xym()), xym())
  expect_error(vctrs::vec_cast(rct(), xym()), class = "vctrs_error_incompatible_type")
  expect_identical(vctrs::vec_proxy(xym(crs = NULL)), data.frame(x = double(), y = double(), m = double()))
  expect_identical(vctrs::vec_restore(data.frame(x = double(), y = double(), m = double()), xym()), xym())
  expect_identical(vctrs::vec_c(xym(), wkt()), wkt())
  expect_identical(vctrs::vec_c(xym(), wkb()), wkb())
  expect_identical(vctrs::vec_c(xym(), xy()), xym())
  expect_identical(vctrs::vec_c(xym(), xyz()), xyzm())
  expect_identical(vctrs::vec_c(xym(), xym()), xym())
  expect_identical(vctrs::vec_c(xym(), xyzm()), xyzm())
  expect_identical(vctrs::vec_c(xym(), rct()), wkb())
})

test_that("vctrs xyzm implementation works", {
  expect_true(vctrs::vec_is(xyzm()))
  expect_identical(vctrs::vec_size(xyzm()), 0L)
  expect_identical(vctrs::vec_cast(wkt(), xyzm()), xyzm())
  expect_identical(vctrs::vec_cast(wkb(), xyzm()), xyzm())
  expect_identical(vctrs::vec_cast(xy(), xyzm()), xyzm())
  expect_identical(vctrs::vec_cast(xyz(), xyzm()), xyzm())
  expect_identical(vctrs::vec_cast(xym(), xyzm()), xyzm())
  expect_identical(vctrs::vec_cast(xyzm(), xyzm()), xyzm())
  expect_error(vctrs::vec_cast(rct(), xyzm()), class = "vctrs_error_incompatible_type")
  expect_identical(vctrs::vec_proxy(xyzm(crs = NULL)), data.frame(x = double(), y = double(), z = double(), m = double()))
  expect_identical(vctrs::vec_restore(data.frame(x = double(), y = double(), z = double(), m = double()), xyzm()), xyzm())
  expect_identical(vctrs::vec_c(xyzm(), wkt()), wkt())
  expect_identical(vctrs::vec_c(xyzm(), wkb()), wkb())
  expect_identical(vctrs::vec_c(xyzm(), xy()), xyzm())
  expect_identical(vctrs::vec_c(xyzm(), xyz()), xyzm())
  expect_identical(vctrs::vec_c(xyzm(), xym()), xyzm())
  expect_identical(vctrs::vec_c(xyzm(), xyzm()), xyzm())
  expect_identical(vctrs::vec_c(xyzm(), rct()), wkb())
})

test_that("vctrs rct implementation works", {
  expect_true(vctrs::vec_is(rct()))
  expect_identical(vctrs::vec_size(rct()), 0L)
  expect_identical(vctrs::vec_cast(rct(), rct()), rct())
  expect_identical(
    vctrs::vec_proxy(rct(crs = NULL)),
    data.frame(xmin = double(), ymin = double(), xmax = double(), ymax = double())
  )
  expect_identical(
    vctrs::vec_restore(data.frame(xmin = double(), ymin = double(), xmax = double(), ymax = double()), rct()),
    rct()
  )

  expect_identical(vctrs::vec_c(rct(), wkb()), wkb())
  expect_identical(vctrs::vec_c(rct(), wkt()), wkt())
  expect_identical(vctrs::vec_c(rct(), xy()), wkb())
  expect_identical(vctrs::vec_c(rct(), xyz()), wkb())
  expect_identical(vctrs::vec_c(rct(), xym()), wkb())
  expect_identical(vctrs::vec_c(rct(), xyzm()), wkb())
  expect_identical(vctrs::vec_c(rct(), rct()), rct())
})

test_that("vctrs crc implementation works", {
  expect_true(vctrs::vec_is(crc()))
  expect_identical(vctrs::vec_size(crc()), 0L)
  expect_identical(vctrs::vec_cast(crc(), crc()), crc())
  expect_identical(
    vctrs::vec_proxy(crc(crs = NULL)),
    data.frame(x = double(), y = double(), r = double())
  )
  expect_identical(
    vctrs::vec_restore(data.frame(x = double(), y = double(), r = double()), crc()),
    crc()
  )

  expect_identical(vctrs::vec_c(crc(), wkb()), wkb())
  expect_identical(vctrs::vec_c(crc(), wkt()), wkt())
  expect_identical(vctrs::vec_c(crc(), xy()), wkb())
  expect_identical(vctrs::vec_c(crc(), xyz()), wkb())
  expect_identical(vctrs::vec_c(crc(), xym()), wkb())
  expect_identical(vctrs::vec_c(crc(), xyzm()), wkb())
  expect_identical(vctrs::vec_c(crc(), crc()), crc())
})

test_that("vec_c() propagates the crs attribute", {
  for (constructor in list(wkb, wkt, xy, xyz, xyzm, rct)) {
    expect_identical(
      vctrs::vec_c(!!constructor(crs = 1234), !!constructor(crs = 1234)),
      !!constructor(crs = 1234)
    )
    expect_identical(
      vctrs::vec_c(!!constructor(crs = 1234), !!constructor()),
      !!constructor(crs = 1234)
    )
    expect_error(
      vctrs::vec_c(!!constructor(crs = 1234), !!constructor(crs = NULL)),
      "are not equal"
    )
  }
})
