
test_that("debugger works on wkb", {
  point <- as.raw(c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
                    0x00, 0x24, 0x40))

  expect_output(wkb_debug(list(point)), "POINT \\[1\\]")

  point_bad_type <- point
  point_bad_type[2] <- as.raw(0xff)
  expect_output(wkb_debug(list(point_bad_type)), "Unrecognized geometry type")
})

test_that("debugger works on wkt stream", {
  expect_output(wkt_streamer_debug("LINESTRING (30 10, 0 0, 0 1)"), "LINESTRING")
  expect_output(wkt_streamer_debug("POLYGON ((30 10, 0 0, 0 1, 30 10))"), "POLYGON")
  expect_output(wkt_streamer_debug("MULTIPOINT (30 10, 0 0)"), "MULTIPOINT")
  expect_output(wkt_streamer_debug("MULTIPOINT ((30 10), (0 0))"), "MULTIPOINT")
})

test_that("debugger works on wkt", {
  expect_output(wkt_debug("LINESTRING (30 10, 0 0, 0 1)"), "LINESTRING")
  expect_output(wkt_debug("POLYGON ((30 10, 0 0, 0 1, 30 10))"), "POLYGON")
  expect_output(wkt_debug("MULTIPOINT (30 10, 0 0)"), "MULTIPOINT")
  expect_output(wkt_debug("MULTIPOINT ((30 10), (0 0))"), "MULTIPOINT")
})

test_that("debugger works on sexp", {
  expect_output(
    wksxp_debug(
      list(
        structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
      )
    ),
    "POINT"
  )
})
