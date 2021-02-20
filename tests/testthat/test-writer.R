
test_that("the wk_writer() generic resolves correct handler", {
  expect_is(wk_writer(wkt()), "wk_wkt_writer")
  expect_is(wk_writer(wkb()), "wk_wkb_writer")
  expect_is(wk_writer(xy()), "wk_xyzm_writer")
  expect_is(wk_writer(structure(list(), class = "sfc")), "wk_sfc_writer")
})

test_that("wkt_writer() works", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(wkt_good, wkt_writer()),
    wkt_good
  )

  expect_error(wk_handle(new_wk_wkt("NOT WKT"), wkt_writer()), "Expected geometry type or 'SRID")
  expect_identical(
    wk_handle(new_wk_wkt("POINT (1 1)"), wkt_writer(precision = 1, trim = FALSE)),
    wkt("POINT (1.0 1.0)")
  )
})

test_that("wkb_writer() works", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(wkb_good, wkb_writer()),
    wkb_good
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wkb_writer()), "Unrecognized geometry type code")
})

test_that("wkb_writer() can generate swapped endian", {
  skip_if_not(wk_platform_endian() == 1)
  wkb_system <- wk_handle(wkt("POINT (1 2)"), wkb_writer(endian = NA))
  wkb_le <- wk_handle(wkt("POINT (1 2)"), wkb_writer(endian = 1))
  wkb_be <- wk_handle(wkt("POINT (1 2)"), wkb_writer(endian = 0))

  expect_identical(as_wkt(wkb_system), wkt("POINT (1 2)"))
  expect_identical(as_wkt(wkb_le), wkt("POINT (1 2)"))
  expect_identical(as_wkt(wkb_be), wkt("POINT (1 2)"))

  expect_identical(wkb_system, wkb_le)
  expect_false(identical(wkb_be, wkb_le))

  expect_identical(
    wkb_be,
    # dput(geos::geos_write_wkb("POINT (1 2)", endian = 0))
    structure(
      list(
        as.raw(
          c(0x00,
            0x00, 0x00, 0x00, 0x01,
            0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
          )
        )
      ),
      class = c("wk_wkb", "wk_vctr")
    )
  )
})

test_that("wkb_writer() reallocates its buffer as needed", {
  expect_identical(
    wk_handle(wkt("POINT (1 2)"), wkb_writer(buffer_size = 0)),
    wk_handle(wkt("POINT (1 2)"), wkb_writer(buffer_size = 1024))
  )
})

test_that("wkb_writer() works with streaming input", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_identical(
    wk_handle(as_wkt(wkb_good), wkb_writer()),
    wkb_good
  )
})

test_that("wkb_writer() errors when the recursion limit is too high", {
  make_really_recursive_geom <- function(n) {
    wkt(paste0(
      c(rep("GEOMETRYCOLLECTION (", n), "POLYGON ((0 1))", rep(")", n)),
      collapse = ""
    ))
  }

  # errors in geometry_start
  expect_error(
    wk_handle(make_really_recursive_geom(31), wkb_writer()),
    "Can't write WKB with maximum"
  )
  # errors in ring_start
  expect_error(
    wk_handle(make_really_recursive_geom(32), wkb_writer()),
    "Can't write WKB with maximum"
  )
})

test_that("xyzm_writer() works", {
  empties <- wkt(
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  expect_identical(
    wk_handle(empties, xyzm_writer()),
    rep(xy(NA, NA), length(empties))
  )

  expect_identical(
    wk_handle(wkt("POINT (0 1)"), xyzm_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("MULTIPOINT ((0 1))"), xyzm_writer()),
    xy(0, 1)
  )

  expect_identical(
    wk_handle(wkt("GEOMETRYCOLLECTION (MULTIPOINT ((0 1)))"), xyzm_writer()),
    xy(0, 1)
  )

  expect_error(
    wk_handle(wkt("LINESTRING (0 1, 1 2)"), xyzm_writer()),
    "Can't convert geometry"
  )

  expect_error(
    wk_handle(wkt("MULTIPOINT (0 1, 1 2)"), xyzm_writer()),
    "contains more than one coordinate"
  )
})
