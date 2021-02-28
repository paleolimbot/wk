
test_that("wk_meta() works", {
  expect_identical(
    wk_meta(wkt(c("POINT (1 2)", NA))),
    data.frame(
      geometry_type = c(1L, NA_integer_),
      size = c(NA_integer_, NA_integer_),
      has_z = c(FALSE, NA),
      has_m = c(FALSE, NA),
      srid = c(NA_integer_, NA_integer_),
      precision = c(0, NA_integer_)
    )
  )

  expect_identical(
    wk_meta(as_wkb(c("POINT (1 2)", NA))),
    data.frame(
      geometry_type = c(1L, NA_integer_),
      size = c(1L, NA_integer_),
      has_z = c(FALSE, NA),
      has_m = c(FALSE, NA),
      srid = c(NA_integer_, NA_integer_),
      precision = c(0, NA_integer_)
    )
  )

  expect_identical(
    wk_meta(as_wkb(c("SRID=1234;POINT (1 2)", NA))),
    data.frame(
      geometry_type = c(1L, NA_integer_),
      size = c(1L, NA_integer_),
      has_z = c(FALSE, NA),
      has_m = c(FALSE, NA),
      srid = c(1234L, NA_integer_),
      precision = c(0, NA_integer_)
    )
  )
})

test_that("wk_vector_meta() works", {
  expect_identical(
    wk_vector_meta(wkt(c("POINT (1 2)", NA))),
    data.frame(
      geometry_type = 0L,
      size = 2L,
      has_z = NA,
      has_m = NA
    )
  )

  # only sf reader has vector meta with embedded dimensions
  skip_if_not_installed("sf")
  expect_identical(
    wk_vector_meta(sf::st_as_sfc("POINT (30 10)")),
    data.frame(
      geometry_type = 1L,
      size = 1L,
      has_z = FALSE,
      has_m = FALSE
    )
  )

  expect_identical(
    wk_vector_meta(sf::st_as_sfc("POINT M (30 10 12)")),
    data.frame(
      geometry_type = 1L,
      size = 1L,
      has_z = FALSE,
      has_m = TRUE
    )
  )

  expect_identical(
    wk_vector_meta(sf::st_as_sfc("POINT Z (30 10 12)")),
    data.frame(
      geometry_type = 1L,
      size = 1L,
      has_z = TRUE,
      has_m = FALSE
    )
  )
})

test_that("geometry type converters work", {
  types_str <- c(
    "point", "linestring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )
  expect_identical(wk_geometry_type(types_str), 1:7)
  expect_identical(wk_geometry_type_label(7:1), rev(types_str))
})
