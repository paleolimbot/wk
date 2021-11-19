
test_that("wk_handle() works for data.frame", {
  expect_error(wk_handle(data.frame(a = 1)), "must have at least one")
  expect_identical(
    wk_handle(data.frame(a = wkt("POINT (0 1)")), wkb_writer()),
    wk_handle(wkt("POINT (0 1)"), wkb_writer())
  )
})

test_that("wk_writer() works for data.frame", {
  expect_s3_class(wk_writer(data.frame(wkt())), "wk_wkt_writer")
  expect_error(wk_writer(data.frame(a = 1)), "must have at least one")
})

test_that("wk_restore() works for data.frame", {
  expect_identical(
    wk_identity(data.frame(a = wkt("POINT (1 2)"))),
    data.frame(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_restore(data.frame(a = wkt("POINT (1 2)")), wkt(c("POINT (1 2)", "POINT (3 4)"))),
    data.frame(a = wkt(c("POINT (1 2)", "POINT (3 4)")), row.names = c("1", "1.1"))
  )

  expect_error(
    wk_restore(data.frame(a = wkt(rep(NA, 3))), wkt(c("POINT (1 2)", "POINT (3 4)"))),
    "Can't assign"
  )

  expect_identical(
    wk_identity(data.frame(a = wkt("POINT (1 2)", crs = 1234))),
    data.frame(a = wkt("POINT (1 2)", crs = 1234))
  )
})

test_that("wk_restore() works for tibble", {
  expect_identical(
    wk_identity(tibble::tibble(a = wkt("POINT (1 2)"))),
    tibble::tibble(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_identity(tibble::tibble(a = wkt("POINT (1 2)", crs = 1234))),
    tibble::tibble(a = wkt("POINT (1 2)", crs = 1234))
  )
})

test_that("wk_translate() works for data.frame", {
  expect_identical(
    wk_translate(as_wkb("POINT (1 2)"), data.frame(a = wkt())),
    data.frame(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_translate(
      tibble::tibble(a = as_wkb("POINT (1 2)")),
      data.frame(a = wkt())
    ),
    data.frame(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_translate(
      data.frame(a = as_wkb("POINT (1 2)")),
      data.frame(a = wkt())
    ),
    data.frame(a = wkt("POINT (1 2)"))
  )
})

test_that("wk_translate() works for tibble::tibble()", {
  expect_identical(
    wk_translate(as_wkb("POINT (1 2)"), tibble::tibble(a = wkt())),
    tibble::tibble(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_translate(
      tibble::tibble(a = as_wkb("POINT (1 2)")),
      tibble::tibble(a = wkt())
    ),
    tibble::tibble(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_translate(
      data.frame(a = as_wkb("POINT (1 2)")),
      tibble::tibble(a = wkt())
    ),
    tibble::tibble(a = wkt("POINT (1 2)"))
  )
})

test_that("wk_handle_slice() works for data.frame", {
  expect_identical(
    wk_handle_slice(data.frame(geom = xy(1:5, 1:5)), xy_writer(), 3, 6),
    xy(3:5, 3:5)
  )
  expect_identical(
    wk_handle_slice(data.frame(geom = xy(1:5, 1:5)), xy_writer(), 0, 2),
    xy(1:2, 1:2)
  )
  expect_identical(
    wk_handle_slice(data.frame(geom = xy(1:5, 1:5)), xy_writer(), 5, 4),
    xy(crs = NULL)
  )
})
