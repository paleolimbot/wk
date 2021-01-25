
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
    unclass(wkt_good)
  )

  expect_error(wk_handle(new_wk_wkt("NOT WKT"), wkt_writer()), "Expected geometry type or 'SRID")
  expect_identical(
    wk_handle(new_wk_wkt("POINT (1 1)"), wkt_writer(precision = 1, trim = FALSE)),
    "POINT (1.0 1.0)"
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
    unclass(wkb_good)
  )

  wkb_bad <- unclass(wkb_good[1])
  wkb_bad[[1]][2] <- as.raw(0xff)
  expect_error(wk_handle(new_wk_wkb(wkb_bad), wkb_writer()), "Unrecognized geometry type code")
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
    unclass(wkb_good)
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
    unclass(rep(xyzm(NA, NA, NA, NA), length(empties)))
  )

  expect_identical(
    wk_handle(wkt("POINT (0 1)"), xyzm_writer()),
    unclass(xyzm(0, 1, NA, NA))
  )

  expect_identical(
    wk_handle(wkt("MULTIPOINT ((0 1))"), xyzm_writer()),
    unclass(xyzm(0, 1, NA, NA))
  )

  expect_identical(
    wk_handle(wkt("GEOMETRYCOLLECTION (MULTIPOINT ((0 1)))"), xyzm_writer()),
    unclass(xyzm(0, 1, NA, NA))
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
