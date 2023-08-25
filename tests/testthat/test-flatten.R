
test_that("wk_flatten() works", {
  expect_identical(
    wk_flatten(wkt(c("MULTIPOINT (0 0, 1 1)", NA))),
    wkt(c("POINT (0 0)", "POINT (1 1)", NA))
  )
  expect_identical(
    wk_flatten(wkt(c("POINT (0 0)", "POINT (1 1)", NA))),
    wkt(c("POINT (0 0)", "POINT (1 1)", NA))
  )
  expect_error(wk_flatten(new_wk_wkt("POINT ENTPY")), "ENTPY")

  # we need this one to trigger a realloc on the details list
  xy_copy <- wk_handle(
    wkt(c(paste0("MULTIPOINT (", paste(1:1025, 1, collapse = ", ") , ")"), "POINT (0 0)")),
    wk_flatten_filter(xy_writer(), add_details = TRUE)
  )
  expect_identical(
    attr(xy_copy, "wk_details"),
    list(feature_id = c(rep(1L, 1025), 2L))
  )
  attr(xy_copy, "wk_details") <- NULL
  expect_identical(xy_copy, c(xy(1:1025, 1), xy(0, 0)))
})

test_that("wk_flatten() propagates attributes", {
  expect_identical(
    wk_flatten(
      wkt("LINESTRING ZM (0 0 0 0, 1 0 0 0)", crs = 1234, geodesic = TRUE)
    ),
    wkt("LINESTRING ZM (0 0 0 0, 1 0 0 0)", crs = 1234, geodesic = TRUE)
  )
})

test_that("wk_flatten() works for polygons", {
  expect_identical(
    wk_flatten(wkt("POLYGON ((0 0, 0 1, 1 0, 0 0))")),
    wkt("POLYGON ((0 0, 0 1, 1 0, 0 0))")
  )
})

test_that("wk_flatten() works for data.frame", {
  expect_equal(
    wk_flatten(data.frame(geom = wkt(c("MULTIPOINT (0 0, 1 1)")))),
    data.frame(geom = wkt(c("POINT (0 0)", "POINT (1 1)"))),
    ignore_attr = TRUE
  )
})

test_that("wk_flatten() communicates correct size and type", {
  expect_identical(
    wk_handle(wkt("POINT (0 0)"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 0L, size = NA_real_, has_z = NA, has_m = NA)
  )

  skip_if_not_installed("sf")
  # need sf because these objects carry vector-level types
  expect_identical(
    wk_handle(sf::st_as_sfc("POINT (0 0)"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 1L, size = 1, has_z = FALSE, has_m = FALSE)
  )
  expect_identical(
    wk_handle(sf::st_as_sfc("MULTIPOINT EMPTY"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 1L, size = NA_real_, has_z = FALSE, has_m = FALSE)
  )
  expect_identical(
    wk_handle(sf::st_as_sfc("MULTILINESTRING EMPTY"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 2L, size = NA_real_, has_z = FALSE, has_m = FALSE)
  )
  expect_identical(
    wk_handle(sf::st_as_sfc("MULTIPOLYGON EMPTY"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 3L, size = NA_real_, has_z = FALSE, has_m = FALSE)
  )
  expect_identical(
    wk_handle(sf::st_as_sfc("GEOMETRYCOLLECTION EMPTY"), wk_flatten_filter(wk_vector_meta_handler())),
    list(geometry_type = 0L, size = NA_real_, has_z = FALSE, has_m = FALSE)
  )
})

test_that("wk_flatten() works for nested collections", {
  expect_identical(
    wk_flatten(
      wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))"),
      max_depth = 3
    ),
    wkt("POINT (0 1)")
  )

  expect_identical(
    wk_flatten(
      wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))"),
      max_depth = 2
    ),
    wkt("GEOMETRYCOLLECTION (POINT (0 1))")
  )

  expect_identical(
    wk_flatten(
      wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))"),
      max_depth = 1
    ),
    wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1)))")
  )

  expect_identical(
    wk_flatten(
      wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))"),
      max_depth = 0
    ),
    wkt("GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (GEOMETRYCOLLECTION (POINT (0 1))))")
  )

  expect_identical(
    wk_handle(
      wkt("GEOMETRYCOLLECTION(MULTIPOINT (30 10, 10 10), LINESTRING (0 0, 1 1), GEOMETRYCOLLECTION EMPTY)"),
      wk_flatten_filter(wkt_writer(), max_depth = 2, add_details = TRUE)
    ),
    structure(
      c("POINT (30 10)", "POINT (10 10)", "LINESTRING (0 0, 1 1)"),
      class = c("wk_wkt", "wk_vctr"),
      wk_details = list(feature_id = c(1L, 1L, 1L))
    )
  )
})
