
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
    wkt(
      c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
        "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
        "GEOMETRYCOLLECTION EMPTY"
      )
    )
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2))), wkt_writer()),
    wkt("POINT (1 2)")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 3))), wkt_writer()),
    wkt("POINT Z (1 2 3)")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 4), "XYM")), wkt_writer()),
    wkt("POINT M (1 2 4)")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_point(c(1, 2, 3, 4))), wkt_writer()),
    wkt("POINT ZM (1 2 3 4)")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_linestring(rbind(c(1, 2), c(2, 3)))), wkt_writer()),
    wkt("LINESTRING (1 2, 2 3)")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0))))), wkt_writer()),
    wkt("POLYGON ((0 0, 1 0, 0 1, 0 0))")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_multipoint(rbind(c(1, 2), c(2, 3)))), wkt_writer()),
    wkt("MULTIPOINT ((1 2), (2 3))")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_multilinestring(list(rbind(c(1, 2), c(2, 3))))), wkt_writer()),
    wkt("MULTILINESTRING ((1 2, 2 3))")
  )

  expect_identical(
    wk_handle(
      sf::st_sfc(sf::st_multipolygon(list(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0)))))),
      wkt_writer()
    ),
    wkt("MULTIPOLYGON (((0 0, 1 0, 0 1, 0 0)))")
  )

  expect_identical(
    wk_handle(sf::st_sfc(sf::st_geometrycollection(list(sf::st_point(c(1, 2))))), wkt_writer()),
    wkt("GEOMETRYCOLLECTION (POINT (1 2))")
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
    unclass(wk_handle(nc_points, xy_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_points)),
    unclass(wk_handle(nc_points, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_lines)),
    unclass(wk_handle(nc_lines, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_polygon)),
    unclass(wk_handle(nc_polygon, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multipoints)),
    unclass(wk_handle(nc_multipoints, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multilines)),
    unclass(wk_handle(nc_multilines, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_multipolygon)),
    unclass(wk_handle(nc_multipolygon, wkb_writer()))
  )

  expect_identical(
    unclass(sf::st_as_binary(nc_collection)),
    unclass(wk_handle(nc_collection, wkb_writer()))
  )
})

test_that("wk_handle.sfc() handles malformed input", {
  skip_if_not_installed("sf")

  bad_sfc <- unclass(sf::st_sfc(sf::st_point(c(1, 2))))

  class(bad_sfc[[1]]) <- "sfg"
  expect_error(
    wk_handle.sfc(bad_sfc, wk_void_handler()),
    "Can't guess dimensions from class of 'sfg'"
  )

  class(bad_sfc[[1]]) <- c("sfg", "XY")
  expect_error(
    wk_handle.sfc(bad_sfc, wk_void_handler()),
    "Unsupported sfg type"
  )

  class(bad_sfc[[1]]) <- c("not_an_sfg", "XY")
  expect_error(
    wk_handle.sfc(bad_sfc, wk_void_handler()),
    "must inherit from 'sfg'"
  )

  bad_sfc[1] <- list(NULL)
  expect_identical(wk_handle.sfc(bad_sfc, wkt_writer()), wkt(NA))
})
