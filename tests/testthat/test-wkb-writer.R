
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
  wkb_bad[[1]][2:3] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wkb_writer()), "Unrecognized geometry type code")
})

test_that("wkb_writer() can generate swapped endian", {
  wkb_system <- wk_handle(wkt("LINESTRING (1 2, 3 4)"), wkb_writer(endian = NA))
  wkb_le <- wk_handle(wkt("LINESTRING (1 2, 3 4)"), wkb_writer(endian = 1))
  wkb_be <- wk_handle(wkt("LINESTRING (1 2, 3 4)"), wkb_writer(endian = 0))

  expect_identical(as_wkt(wkb_system), wkt("LINESTRING (1 2, 3 4)"))
  expect_identical(as_wkt(wkb_le), wkt("LINESTRING (1 2, 3 4)"))
  expect_identical(as_wkt(wkb_be), wkt("LINESTRING (1 2, 3 4)"))

  expect_false(identical(wkb_be, wkb_le))

  expect_identical(
    wkb_be,
    # dput(geos::geos_write_wkb("LINESTRING (1 2, 3 4)", endian = 0))
    structure(
      list(
        as.raw(
          c(0x00,
            0x00, 0x00, 0x00, 0x02,
            0x00, 0x00, 0x00, 0x02,
            0x3f, 0xf0, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x40, 0x08, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            0x40, 0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00
          )
        )
      ),
      class = c("wk_wkb", "wk_vctr")
    )
  )

  expect_identical(
    wkb_le,
    # dput(geos::geos_write_wkb("LINESTRING (1 2, 3 4)", endian = 1))
    structure(
      list(
        as.raw(
          c(0x01,
            0x02, 0x00, 0x00, 0x00,
            0x02, 0x00, 0x00, 0x00,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0xf0, 0x3f,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x08, 0x40,
            0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x10, 0x40
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

test_that("wkb_writer() works for a vector of indeterminate length", {
  long_xy <- as_wkt(xy(runif(2048), runif(2048)))
  expect_identical(
    handle_wkt_without_vector_size(long_xy, wkb_writer()),
    wk_handle(long_xy, wkb_writer())
  )
})
