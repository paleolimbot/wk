
test_that("sf CRS objects can be compared", {
  skip_if_not_installed("sf")

  expect_true(wk_crs_equal(sf::st_crs(4326), 4326))
  expect_true(wk_crs_equal(sf::st_crs(4326), 4326L))
  expect_true(wk_crs_equal(sf::st_crs(NA), NULL))
  expect_true(wk_crs_equal(NULL, sf::st_crs(NA)))
})

test_that("conversion from sf to wkt works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  expect_is(as_wkt(sfc), "wk_wkt")
  expect_identical(
    as.character(as_wkt(sfc)),
    c("POINT EMPTY", "POINT (0 1)")
  )
  expect_identical(wk_crs(as_wkt(sfc)), sf::st_crs(sfc))

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    as.character(as_wkt(sf)),
    c("POINT EMPTY", "POINT (0 1)")
  )
  expect_identical(wk_crs(as_wkt(sf)), sf::st_crs(sf))
})

test_that("conversion from sf to wkb works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  expect_is(as_wkb(sfc), "wk_wkb")
  expect_identical(
    as.character(as_wkt(as_wkb(sfc))),
    c("POINT (nan nan)", "POINT (0 1)")
  )
  expect_identical(wk_crs(as_wkb(sfc)), sf::st_crs(sfc))

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    as.character(as_wkt(as_wkb(sf))),
    c("POINT (nan nan)", "POINT (0 1)")
  )
  expect_identical(wk_crs(as_wkb(sf)), sf::st_crs(sf))
})

test_that("conversion from sf to xy works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)))
  expect_is(as_xy(sfc), "wk_xy")
  expect_identical(as_xy(sfc), xy(c(NA, 0), c(NA, 1)))

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(as_xy(sf), xy(c(NA, 0), c(NA, 1)))

  expect_identical(as_xy(sf::st_sfc()), xy(crs = NULL))
  expect_error(as_xy(sf::st_sfc(sf::st_linestring())), "Can't create xy")

  # check all dimensions
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3, 4), dim = "XYZM"))), xyzm(1, 2, 3, 4))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3), dim = "XYZ"))), xyz(1, 2, 3))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3), dim = "XYM"))), xym(1, 2, 3))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2)))), xy(1, 2))
})

test_that("conversion from bbox to rct works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(c(2, 3)), sf::st_point(c(0, 1)))
  expect_identical(as_rct(sf::st_bbox(sfc)), rct(0, 1, 2, 3))
})

test_that("conversion to sf works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  wkb <- as_wkb(c("POINT EMPTY", "POINT (0 1)"), crs = 4326)
  wkt <- as_wkt(c("POINT EMPTY", "POINT (0 1)"), crs = 4326)

  expect_equal(sf::st_as_sf(wkb), sf)
  expect_equal(sf::st_as_sfc(wkb), sfc)
  expect_equal(sf::st_as_sf(wkt), sf)
  expect_equal(sf::st_as_sfc(wkt), sfc)

  # xy has no SRID
  expect_equal(sf::st_as_sf(xy(c(NA, 0), c(NA, 1), crs = 4326)), sf)
  expect_equal(sf::st_as_sfc(xy(c(NA, 0), c(NA, 1), crs = 4326)), sfc)

  # rct can only generate rectangles
  expect_equal(
    sf::st_as_sfc(rct(1, 2, 3, 4, crs = 4326)),
    sf::st_as_sfc(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4), crs =  4326))
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
    wk_handle(sf::st_sfc(sf::st_multipolygon(list(list(rbind(c(0, 0), c(1, 0), c(0, 1), c(0, 0)))))), wkt_writer()),
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
    xyzm_trim(wk_handle(nc_points, xyzm_writer()), FALSE, FALSE)
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

test_that("wk_handle.sfg works", {
  skip_if_not_installed("sf")
  expect_identical(
    wk_handle(wkt("POINT (1 2)"), wkb_writer()),
    wk_handle(sf::st_point(c(1, 2)), wkb_writer())
  )
})

test_that("wk_handle.bbox works", {
  skip_if_not_installed("sf")

  expect_identical(
    wk_handle(sf::st_bbox(sf::st_linestring(rbind(c(0, 1), c(2, 3)))), wkb_writer()),
    wk_handle(rct(0, 1, 2, 3), wkb_writer())
  )
})


test_that("sfc_writer() works with fixed-length input", {
  skip_if_not_installed("sf")

  # zero-length
  expect_identical(wk_handle(wkb(), sfc_writer()), sf::st_sfc())

  # empties (equal because of NaN/NA difference for POINT)
  expect_equal(
    wk_handle(
      as_wkb(
        c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
          "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
          "GEOMETRYCOLLECTION EMPTY"
        )
      ),
      sfc_writer()
    ),
    sf::st_sfc(
      sf::st_point(), sf::st_linestring(), sf::st_polygon(),
      sf::st_multipoint(), sf::st_multilinestring(), sf::st_multipolygon(),
      sf::st_geometrycollection()
    )
  )

  expect_identical(
    wk_handle(as_wkb("POINT (1 1)"), sfc_writer()),
    sf::st_sfc(sf::st_point(c(1, 1)))
  )

  expect_identical(
    wk_handle(as_wkb("LINESTRING (1 2, 3 4)"), sfc_writer()),
    sf::st_sfc(sf::st_linestring(rbind(c(1, 2), c(3, 4))))
  )

  expect_identical(
    wk_handle(as_wkb("POLYGON ((0 0, 0 1, 1 0, 0 0))"), sfc_writer()),
    sf::st_sfc(sf::st_polygon(list(rbind(c(0, 0), c(0, 1), c(1, 0), c(0, 0)))))
  )

  expect_identical(
    wk_handle(as_wkb("MULTIPOINT ((1 2), (3 4))"), sfc_writer()),
    sf::st_sfc(sf::st_multipoint(rbind(c(1, 2), c(3, 4))))
  )

  expect_identical(
    wk_handle(as_wkb("MULTILINESTRING ((1 1, 2 2), (2 2, 3 4))"), sfc_writer()),
    sf::st_sfc(
      sf::st_multilinestring(
        list(rbind(c(1, 1), c(2, 2)), rbind(c(2, 2), c(3, 4)))
      )
    )
  )

  expect_identical(
    wk_handle(
      as_wkb("MULTIPOLYGON (((0 0, 0 1, 1 0, 0 0)), ((0 0, 0 -2, -1 0, 0 0)))"),
      sfc_writer()
    ),
    sf::st_sfc(
      sf::st_multipolygon(
        list(
          list(rbind(c(0, 0), c(0, 1), c(1, 0), c(0, 0))),
          list(rbind(c(0, 0), c(0, -2), c(-1, 0), c(0, 0)))
        )
      )
    )
  )

  expect_identical(
    wk_handle(as_wkb("GEOMETRYCOLLECTION (POINT (1 1), LINESTRING (1 1, 2 2))"), sfc_writer()),
    sf::st_sfc(
      sf::st_geometrycollection(
        list(
          sf::st_point(c(1, 1)),
          sf::st_linestring(rbind(c(1, 1), c(2, 2)))
        )
      )
    )
  )
})

test_that("sfc_writer() turns NULLs into EMPTY", {
  all_types <- as_wkb(
    c("POINT EMPTY", "LINESTRING EMPTY", "POLYGON EMPTY",
      "MULTIPOINT EMPTY", "MULTILINESTRING EMPTY", "MULTIPOLYGON EMPTY",
      "GEOMETRYCOLLECTION EMPTY"
    )
  )

  for (i in seq_along(all_types)) {
    expect_equal(
      wk_handle(c(all_types[i], wkb(list(NULL))), sfc_writer()),
      wk_handle(c(all_types[i], all_types[i]), sfc_writer())
    )
  }

  expect_identical(
    wk_handle(c(all_types[1:2], wkb(list(NULL))), sfc_writer()),
    wk_handle(c(all_types[1:2], as_wkb("GEOMETRYCOLLECTION EMPTY")), sfc_writer())
  )
})

test_that("sfc_writer() reproduces all basic geometry types for WKB input", {
  skip_if_not_installed("sf")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_multipolygon <- sf::st_set_crs(nc$geometry, NA)
  nc_multilines <- sf::st_boundary(nc_multipolygon)
  nc_multipoints <- sf::st_cast(nc_multilines, "MULTIPOINT")
  nc_polygon <- sf::st_cast(nc_multipolygon, "POLYGON")
  nc_lines <- sf::st_cast(nc_multilines, "LINESTRING")
  nc_points <- sf::st_cast(nc_lines, "POINT")
  collection_list <- nc_multipolygon
  attributes(collection_list) <- NULL
  nc_collection <- sf::st_sfc(sf::st_geometrycollection(collection_list))

  attr(nc_multipoints, "ids") <- NULL
  attr(nc_polygon, "ids") <- NULL
  attr(nc_lines, "ids") <- NULL
  attr(nc_points, "ids") <- NULL

  expect_identical(
    wk_handle(as_wkb(nc_multipolygon), sfc_writer()),
    nc_multipolygon
  )

  expect_identical(
    wk_handle(as_wkb(nc_multilines), sfc_writer()),
    nc_multilines
  )

  expect_identical(
    wk_handle(as_wkb(nc_multipoints), sfc_writer()),
    nc_multipoints
  )

  expect_identical(
    wk_handle(as_wkb(nc_polygon), sfc_writer()),
    nc_polygon
  )

  expect_identical(
    wk_handle(as_wkb(nc_lines), sfc_writer()),
    nc_lines
  )

  expect_identical(
    wk_handle(as_wkb(nc_points), sfc_writer()),
    nc_points
  )

  expect_identical(
    wk_handle(as_wkb(nc_collection), sfc_writer()),
    nc_collection
  )
})

test_that("sfc_writer() reproduces all basic geometry types for WKT input", {
  skip_if_not_installed("sf")

  nc <- sf::read_sf(system.file("shape/nc.shp", package = "sf"))
  nc_multipolygon <- sf::st_set_crs(nc$geometry, NA)
  nc_multilines <- sf::st_boundary(nc_multipolygon)
  nc_multipoints <- sf::st_cast(nc_multilines, "MULTIPOINT")
  nc_polygon <- sf::st_cast(nc_multipolygon, "POLYGON")
  nc_lines <- sf::st_cast(nc_multilines, "LINESTRING")
  nc_points <- sf::st_cast(nc_lines, "POINT")
  collection_list <- nc_multipolygon
  attributes(collection_list) <- NULL
  nc_collection <- sf::st_sfc(sf::st_geometrycollection(collection_list))

  attr(nc_multipoints, "ids") <- NULL
  attr(nc_polygon, "ids") <- NULL
  attr(nc_lines, "ids") <- NULL
  attr(nc_points, "ids") <- NULL

  expect_equal(
    wk_handle(as_wkt(nc_multipolygon), sfc_writer()),
    nc_multipolygon
  )

  expect_equal(
    wk_handle(as_wkt(nc_multilines), sfc_writer()),
    nc_multilines
  )

  expect_equal(
    wk_handle(as_wkt(nc_multipoints), sfc_writer()),
    nc_multipoints
  )

  expect_equal(
    wk_handle(as_wkt(nc_polygon), sfc_writer()),
    nc_polygon
  )

  expect_equal(
    wk_handle(as_wkt(nc_lines), sfc_writer()),
    nc_lines
  )

  expect_equal(
    wk_handle(as_wkt(nc_points), sfc_writer()),
    nc_points
  )

  expect_equal(
    wk_handle(as_wkt(nc_collection), sfc_writer()),
    nc_collection
  )
})

test_that("wk_translate.sfc() works", {
  skip_if_not_installed("sf")

  expect_identical(
    wk_translate(wkt("POINT (1 2)", crs = 4326), sf::st_sfc(crs = 4326)),
    sf::st_sfc(sf::st_point(c(1, 2)), crs = 4326)
  )
})
