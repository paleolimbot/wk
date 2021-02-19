
test_that("wk_handler class works", {
  expect_true(is_wk_handler(wk_void_handler()))
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_output(print(wk_void_handler()), "wk_void_handler")
  expect_is(as_wk_handler(wk_void_handler), "wk_void_handler")
})

test_that("wk_handle.wk_xy() works", {
  expect_identical(
    wk_handle(xy(c(NA, 2, 3, NA), c(NA, NA, 4, 5)), wkt_writer()),
    wkt(c("POINT EMPTY", "POINT (2 nan)", "POINT (3 4)", "POINT (nan 5)"))
  )

  expect_identical(
    wk_handle(xyz(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    wkt(c("POINT EMPTY", "POINT Z (2 nan nan)", "POINT Z (3 4 nan)", "POINT Z (nan 5 nan)"))
  )

  expect_identical(
    wk_handle(xym(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    wkt(c("POINT EMPTY", "POINT M (2 nan nan)", "POINT M (3 4 nan)", "POINT M (nan 5 nan)"))
  )

  expect_identical(
    wk_handle(xyzm(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA), c(NA, rep(1, 3))), wkt_writer()),
    wkt(c("POINT EMPTY", "POINT ZM (2 nan nan 1)", "POINT ZM (3 4 nan 1)", "POINT ZM (nan 5 nan 1)"))
  )
})

test_that("wk_handle.wk_rct() works", {
  expect_identical(
    wk_handle(rct(c(1, NA, Inf, 0), c(2, NA, 0, Inf), c(3, NA, 1, 1), c(4, NA, 1, 1)), wkt_writer()),
    wkt(c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", "POLYGON EMPTY", "POLYGON EMPTY", "POLYGON EMPTY"))
  )
})

test_that("wk_handle.wk_crc() works", {
  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(precision = 2), n_segments = 4),
    wkt("POLYGON ((4 2, 1 5, -2 2, 1 -1, 4 2))")
  )

  crc_wkb <- wk_handle(crc(1, 2, 3), wkb_writer(), n_segments = 50L)
  # endian + type + size + ring size + 51 coords
  expect_length(unclass(crc_wkb)[[1]], 1 + 4 + 4 + 4 + 51 * 8 * 2)

  # check vectorization of n_segments
  crc_wkb2 <- wk_handle(crc(1:5, 2, 3), wkb_writer(), n_segments = 50:54)
  crc_wkb2_lengths <- vapply(unclass(crc_wkb2), length, integer(1))
  expect_equal(crc_wkb2_lengths, 1 + 4 + 4 + 4 + ((51:55) * 8 * 2))

  # check emptiness of NA circle
  expect_identical(as_wkt(crc(NA, NA, NA)), wkt("POLYGON EMPTY"))

  # check options for circle resolution
  prev_opt <- options(wk.crc_n_segments = 4)
  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(precision = 2)),
    wkt("POLYGON ((4 2, 1 5, -2 2, 1 -1, 4 2))")
  )
  options(prev_opt)

  prev_opt <- options(wk.crc_resolution = 100)
  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(precision = 2)),
    wkt("POLYGON ((4 2, 1 5, -2 2, 1 -1, 4 2))")
  )
  options(prev_opt)

  # check invalid options
  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(), n_segments = 0),
    wk_handle(crc(1, 2, 3), wkt_writer(), n_segments = 4)
  )
  expect_identical(
    wk_handle(crc(1, 2, 3), wkt_writer(), n_segments = NA),
    wk_handle(crc(1, 2, 3), wkt_writer(), n_segments = 4)
  )
  expect_error(wk_handle(crc(1, 2, 3), wkt_writer(), n_segments = double()), "must be length 1")
})
