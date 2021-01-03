
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
