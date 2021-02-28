
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
