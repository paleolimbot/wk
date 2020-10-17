
test_that("conversion from sf to wkt works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  expect_is(as_wkt(sfc), "wk_wkt")
  expect_identical(
    as.character(as_wkt(sfc)),
    c("POINT EMPTY", "POINT (0 1)")
  )

  expect_identical(
    as.character(as_wkt(sfc, include_srid = TRUE)),
    c("POINT EMPTY", "SRID=4326;POINT (0 1)")
  )

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    as.character(as_wkt(sf)),
    c("POINT EMPTY", "POINT (0 1)")
  )
})

test_that("conversion from sf to wkb works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  expect_is(as_wkb(sfc), "wk_wkb")
  expect_identical(
    as.character(as_wkt(as_wkb(sfc))),
    c("POINT (nan nan)", "POINT (0 1)")
  )

  expect_identical(
    as.character(as_wkt(as_wkb(sfc, include_srid = TRUE))),
    c("SRID=4326;POINT (nan nan)", "SRID=4326;POINT (0 1)")
  )

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    as.character(as_wkt(as_wkb(sf))),
    c("POINT (nan nan)", "POINT (0 1)")
  )
})

test_that("conversion from sf to wksxp works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  expect_is(as_wksxp(sfc), "wk_wksxp")
  expect_identical(
    as.character(as_wkt(as_wksxp(sfc))),
    c("POINT EMPTY", "POINT (0 1)")
  )

  expect_identical(
    as.character(as_wkt(as_wksxp(sfc, include_srid = TRUE))),
    c("POINT EMPTY", "SRID=4326;POINT (0 1)")
  )

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(
    as.character(as_wkt(as_wksxp(sf))),
    c("POINT EMPTY", "POINT (0 1)")
  )
})

test_that("conversion from sf to xy works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)))
  expect_is(as_xy(sfc), "wk_xy")
  expect_identical(as_xy(sfc), xy(c(NA, 0), c(NA, 1)))

  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  expect_identical(as_xy(sf), xy(c(NA, 0), c(NA, 1)))

  expect_identical(as_xy(sf::st_sfc()), xy())
  expect_error(as_xy(sf::st_sfc(sf::st_linestring())), "Can't create xy")

  # check all dimensions
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3, 4), dim = "XYZM"))), xyzm(1, 2, 3, 4))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3), dim = "XYZ"))), xyz(1, 2, 3))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2, 3), dim = "XYM"))), xym(1, 2, 3))
  expect_identical(as_xy(sf::st_sfc(sf::st_point(c(1, 2)))), xy(1, 2))
})

test_that("conversion to sf works", {
  skip_if_not_installed("sf")

  sfc <- sf::st_sfc(sf::st_point(), sf::st_point(c(0, 1)), crs = 4326)
  sf <- sf::st_as_sf(new_data_frame(list(geometry = sfc)))
  wkb <- as_wkb(c("POINT EMPTY", "SRID=4326;POINT (0 1)"))
  wkt <- as_wkt(c("POINT EMPTY", "SRID=4326;POINT (0 1)"))
  wksxp <- as_wksxp(c("POINT EMPTY", "SRID=4326;POINT (0 1)"))

  expect_equal(sf::st_as_sf(wkb), sf)
  expect_equal(sf::st_as_sfc(wkb), sfc)
  expect_equal(sf::st_as_sf(wkt), sf)
  expect_equal(sf::st_as_sfc(wkt), sfc)
  expect_equal(sf::st_as_sf(wksxp), sf)
  expect_equal(sf::st_as_sfc(wksxp), sfc)

  # xy has no SRID
  expect_equal(sf::st_as_sf(xy(c(NA, 0), c(NA, 1))), sf::st_set_crs(sf, NA))
  expect_equal(sf::st_as_sfc(xy(c(NA, 0), c(NA, 1))), sf::st_set_crs(sfc, NA))

  # rct can only generate rectangles
  expect_equal(
    sf::st_as_sfc(rct(1, 2, 3, 4)),
    sf::st_as_sfc(sf::st_bbox(c(xmin = 1, ymin = 2, xmax = 3, ymax = 4)))
  )
})
