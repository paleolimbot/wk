
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
