
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
})
