
test_that("handlers can be created", {
  expect_is(wk_void_handler(), "wk_void_handler")
  expect_is(wk_void_handler(), "wk_handler")
})

test_that("wk_handler class works", {
  expect_true(is_wk_handler(wk_void_handler()))
  handler <- wk_void_handler()
  expect_identical(as_wk_handler(handler), handler)
  expect_output(print(wk_void_handler()), "wk_void_handler")
})

test_that("void handlers do nothing", {
  wkb_good <- as_wkb(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_null(wk_handle(wkb_good, wk_void_handler()))

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wk_void_handler()), "Unrecognized geometry type code")
})

test_that("void handlers cannot be re-used", {
  handler <- wk_void_handler()
  expect_null(wk_handle(as_wkb("POINT (1 1)"), handler))
  expect_error(wk_handle(as_wkb("POINT (1 1)"), handler), "Can't re-use this wk_handler")
})

test_that("void handlers do nothing when passed to the wkt handler", {
  wkt_good <- as_wkt(
    c(
      "POINT (1 1)", "LINESTRING (1 1, 2 2)", "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "MULTIPOINT ((1 1))", "MULTILINESTRING ((1 1, 2 2), (2 2, 3 3))",
      "MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -1, -1 0, 0 0)))",
      "GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"
    )
  )

  expect_null(wk_handle(wkt_good, wk_void_handler()))

  wkt_bad <- new_wk_wkt("NOT WKT")
  expect_error(wk_handle(wkt_bad, wk_void_handler()), "Expected geometry type or 'SRID='")
})

test_that("void handlers cannot be re-used when called from C++", {
  handler <- wk_void_handler()
  expect_null(wk_handle(as_wkt("POINT (1 1)"), handler))
  expect_error(wk_handle(as_wkt("POINT (1 1)"), handler), "Can't re-use this wk_handler")
})


test_that("wk_handle.wk_xy() works", {
  expect_identical(
    wk_handle(xy(c(NA, 2, 3, NA), c(NA, NA, 4, 5)), wkt_writer()),
    c("POINT EMPTY", "POINT (2 nan)", "POINT (3 4)", "POINT (nan 5)")
  )

  expect_identical(
    wk_handle(xyz(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT Z (2 nan nan)", "POINT Z (3 4 nan)", "POINT Z (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xym(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA)), wkt_writer()),
    c("POINT EMPTY", "POINT M (2 nan nan)", "POINT M (3 4 nan)", "POINT M (nan 5 nan)")
  )

  expect_identical(
    wk_handle(xyzm(c(NA, 2, 3, NA), c(NA, NA, 4, 5), c(NA, NA, NA, NA), c(NA, rep(1, 3))), wkt_writer()),
    c("POINT EMPTY", "POINT ZM (2 nan nan 1)", "POINT ZM (3 4 nan 1)", "POINT ZM (nan 5 nan 1)")
  )
})

test_that("wk_handle.wk_rct() works", {
  expect_identical(
    wk_handle(rct(c(1, NA, Inf, 0), c(2, NA, 0, Inf), c(3, NA, 1, 1), c(4, NA, 1, 1)), wkt_writer()),
    c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", "POLYGON EMPTY", "POLYGON EMPTY", "POLYGON EMPTY")
  )
})

test_that("wk_handle.sfc() works", {
  skip_if_not_installed("sf")

  expect_identical(
    wk_handle(
      sf::st_sfc(
        sf::st_point(), sf::st_linestring(), sf::st_polygon(),
        sf::st_multipoint(), sf::st_multilinestring(), sf::st_multipolygon(),
        sf::st_geometrycollection()
      ),
      wkt_writer()
    ),
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2))), wkt_writer()),
    "POINT (1 2)"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 3))), wkt_writer()),
    "POINT Z (1 2 3)"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 4), "XYM")), wkt_writer()),
    "POINT M (1 2 4)"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 3, 4))), wkt_writer()),
    "POINT ZM (1 2 3 4)"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_linestring(rbind(c(1, 2), c(2, 3)))), wkt_writer()),
    "LINESTRING (1 2, 2 3)"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0))))), wkt_writer()),
    "POLYGON ((0 0, 1 0, 0 1, 0 0))"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_multipoint(rbind(c(1, 2), c(2, 3)))), wkt_writer()),
    "MULTIPOINT ((1 2), (2 3))"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_multilinestring(list(rbind(c(1, 2), c(2, 3))))), wkt_writer()),
    "MULTILINESTRING ((1 2, 2 3))"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_multipolygon(list(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0)))))), wkt_writer()),
    "MULTIPOLYGON (((0 0, 1 0, 0 1, 0 0)))"
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2))))), wkt_writer()),
    "GEOMETRYCOLLECTION (POINT (1 2))"
  )
})

test_that("wk_handle.sfc() generates same WKB as st_as_binary", {
  skip_if_not_installed("sf")

  nc_multipolygon <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))$geometry
  nc_multilines <- sf::st_boundary(nc_multipolygon)
  nc_multipoints <- sf::st_cast(nc_multilines, "MULTIPOINT")
  nc_polygon <- sf::st_cast(nc_multipolygon, "POLYGON")
  nc_lines <- sf::st_cast(nc_multilines, "LINESTRING")
  nc_points <- sf::st_cast(nc_lines, "POINT")
  nc_collection <- sf::st_sfc(sf::st_geometrycollection(nc_multipolygon))

  expect_identical(
    unclass(as_xy(sf::st_coordinates(nc_points))),
    xyzm_trim(wk_handle(nc_points, xyzm_writer()), FALSE, FALSE)
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_points)),
    wk_handle(nc_points, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_lines)),
    wk_handle(nc_lines, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_polygon)),
    wk_handle(nc_polygon, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multipoints)),
    wk_handle(nc_multipoints, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multilines)),
    wk_handle(nc_multilines, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multipolygon)),
    wk_handle(nc_multipolygon, wkb_writer())
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_collection)),
    wk_handle(nc_collection, wkb_writer())
  )
})
