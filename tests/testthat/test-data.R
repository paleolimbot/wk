
test_that("all examples can be created with default arguments", {
  for (which in names(wk::wk_example_wkt)) {
    expect_s3_class(wk_example(!! which), "wk_wkt")
  }
})

test_that("requested example crs is respected", {
  expect_identical(
    wk_crs(wk_example(crs = "EPSG:1234")),
    "EPSG:1234"
  )
})

test_that("requested example edges field is respected", {
  spherical <- wk_example(geodesic = TRUE)
  expect_true(wk_is_geodesic(spherical))
})
