
test_that("crs setting and getting works on wk_vctr",  {
  x <- new_wk_wkt()
  expect_null(wk_crs(x))

  x <- wk_set_crs(x, 4326)
  expect_identical(wk_crs(x), 4326)

  wk_crs(x) <- 26920
  expect_identical(wk_crs(x), 26920)
})

test_that("crs setting and getting works on wk_rcrd",  {
  x <- new_wk_xy()
  expect_null(wk_crs(x))

  x <- wk_set_crs(x, 4326)
  expect_identical(wk_crs(x), 4326)

  wk_crs(x) <- 26920
  expect_identical(wk_crs(x), 26920)
})

test_that("geodesic getting and setting works for wkb", {
  x <- new_wk_wkb()
  expect_false(wk_is_geodesic(x))
  x <- wk_set_geodesic(x, TRUE)
  expect_true(wk_is_geodesic(x))
  wk_is_geodesic(x) <- FALSE
  expect_false(wk_is_geodesic(x))
  expect_null(attr(x, "geodesic"))

  wk_is_geodesic(x) <- wk_geodesic_inherit()
  expect_identical(wk_is_geodesic(x), NA)

  expect_error(wk_set_geodesic(x, "fish"), "must be TRUE, FALSE, or NA")
})

test_that("geodesic getting and setting works for wkt", {
  x <- new_wk_wkt()
  expect_false(wk_is_geodesic(x))
  x <- wk_set_geodesic(x, TRUE)
  expect_true(wk_is_geodesic(x))
  wk_is_geodesic(x) <- FALSE
  expect_false(wk_is_geodesic(x))
  expect_null(attr(x, "geodesic"))

  wk_is_geodesic(x) <- wk_geodesic_inherit()
  expect_identical(wk_is_geodesic(x), NA)

  expect_error(wk_set_geodesic(x, "fish"), "must be TRUE, FALSE, or NA")
})

test_that("geodesic setting gives a warning when this isn't supported", {
  expect_warning(wk_set_geodesic(xy(), TRUE), "for object of class 'wk_xy'")
})

test_that("wk_geodesic_output() works", {
  expect_identical(
    wk_is_geodesic_output(wkt(geodesic = FALSE), wkt(geodesic = FALSE)),
    FALSE
  )

  expect_identical(
    wk_is_geodesic_output(wkt(geodesic = TRUE), wkt(geodesic = TRUE)),
    TRUE
  )

  expect_identical(
    wk_is_geodesic_output(wkt(geodesic = wk_geodesic_inherit()), wkt(geodesic = FALSE)),
    FALSE
  )

  expect_identical(
    wk_is_geodesic_output(wkt(geodesic = FALSE), wkt(geodesic = wk_geodesic_inherit())),
    FALSE
  )

  expect_error(
    wk_is_geodesic_output(wkt(geodesic = TRUE), wkt(geodesic = FALSE)),
    "differing values"
  )

  expect_error(
    wk_is_geodesic_output(wkt(geodesic = FALSE), wkt(geodesic = TRUE)),
    "differing values"
  )
})

test_that("crs comparison works", {
  expect_true(wk_crs_equal(NULL, NULL))
  expect_false(wk_crs_equal(NULL, "something"))
  expect_false(wk_crs_equal("something", NULL))

  expect_true(wk_crs_equal("something", "something"))
  expect_false(wk_crs_equal("something", "something_else"))

  expect_true(wk_crs_equal(1234, 1234L))
  expect_true(wk_crs_equal(1234L, 1234))

  expect_false(wk_crs_equal(NULL, 1234))
})

test_that("crs output computing works", {
  x <- wkt("POINT (0 0)", crs = NULL)

  expect_identical(wk_crs_output(x, x), NULL)
  expect_identical(wk_crs_output(x, wk_set_crs(x, wk_crs_inherit())), NULL)
  expect_identical(wk_crs_output(wk_set_crs(x, wk_crs_inherit()), x), NULL)
  expect_identical(
    wk_crs_output(wk_set_crs(x, wk_crs_inherit()), wk_set_crs(x, wk_crs_inherit())),
    wk_crs_inherit()
  )
  expect_identical(wk_crs_output(wk_set_crs(x, 1), wkt()), 1)
  expect_identical(wk_crs_output(wkt(), wk_set_crs(x, 1)), 1)
  expect_error(wk_crs_output(wk_set_crs(x, 1), wk_set_crs(x, 2)), "are not equal")
})

test_that("crs_proj_definition() works", {
  expect_identical(wk_crs_proj_definition(NULL), NA_character_)
  expect_identical(wk_crs_proj_definition(wk_crs_inherit()), NA_character_)
  expect_identical(wk_crs_proj_definition(1234), "EPSG:1234")
  expect_identical(wk_crs_proj_definition(NA_real_), NA_character_)
  expect_identical(wk_crs_proj_definition(1234L), "EPSG:1234")
  expect_identical(wk_crs_proj_definition(NA_integer_), NA_character_)
  expect_identical(wk_crs_proj_definition("EPSG:1234"), "EPSG:1234")
  expect_identical(wk_crs_proj_definition(NA_character_), NA_character_)
})

test_that("wk_crs_projjson() works", {
  expect_identical(wk_crs_projjson(NULL), NA_character_)
  expect_identical(wk_crs_projjson("{probably json}"), "{probably json}")
  expect_identical(wk_crs_projjson("probably not json"), NA_character_)
  expect_identical(wk_crs_projjson("EPSG:1234"), NA_character_)
  expect_match(wk_crs_projjson("EPSG:4326"), "GeographicCRS")
  expect_match(wk_crs_projjson(4326), "GeographicCRS")
})

test_that("wk_crs_longlat() works for common datums", {
  expect_identical(wk_crs_longlat(), "OGC:CRS84")
  expect_identical(wk_crs_longlat(wk_crs_inherit()), "OGC:CRS84")
  expect_identical(wk_crs_longlat("OGC:CRS84"), "OGC:CRS84")
  expect_identical(wk_crs_longlat("EPSG:4326"), "OGC:CRS84")
  expect_identical(wk_crs_longlat("WGS84"), "OGC:CRS84")

  expect_identical(wk_crs_longlat("OGC:CRS83"), "OGC:CRS83")
  expect_identical(wk_crs_longlat("EPSG:4269"), "OGC:CRS83")
  expect_identical(wk_crs_longlat("NAD83"), "OGC:CRS83")

  expect_identical(wk_crs_longlat("OGC:CRS27"), "OGC:CRS27")
  expect_identical(wk_crs_longlat("EPSG:4267"), "OGC:CRS27")
  expect_identical(wk_crs_longlat("NAD27"), "OGC:CRS27")

  expect_identical(wk_crs_longlat(), "OGC:CRS84")
  expect_identical(wk_crs_longlat(), "OGC:CRS84")
  expect_identical(wk_crs_longlat(), "OGC:CRS84")
  expect_identical(wk_crs_longlat(), "OGC:CRS84")

  expect_error(wk_crs_longlat("not a crs"), "Can't guess authority-compliant")
})

test_that("wk_crs_inherit() prints as expected", {
  expect_match(format(wk_crs_inherit()), "wk_crs_inherit")
  expect_output(print(wk_crs_inherit()), "wk_crs_inherit")
})
