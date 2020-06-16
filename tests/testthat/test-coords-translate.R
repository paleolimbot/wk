
test_that("coords_*_translate_wkt() works", {
  # point
  expect_identical(coords_point_translate_wkt(double(), double()), character(0))
  expect_identical(coords_point_translate_wkt(NA, NA), "POINT EMPTY")
  expect_identical(
    coords_point_translate_wkt(1:3, 2:4),
    c("POINT (1 2)", "POINT (2 3)", "POINT (3 4)")
  )
  expect_identical(
    coords_point_translate_wkt(1:3, 2:4, 3:5),
    c("POINT Z (1 2 3)", "POINT Z (2 3 4)", "POINT Z (3 4 5)")
  )
  expect_identical(
    coords_point_translate_wkt(1:3, 2:4, NA, 3:5),
    c("POINT M (1 2 3)", "POINT M (2 3 4)", "POINT M (3 4 5)")
  )
  expect_identical(
    coords_point_translate_wkt(1:3, 2:4, 3:5, 4:6),
    c("POINT ZM (1 2 3 4)", "POINT ZM (2 3 4 5)", "POINT ZM (3 4 5 6)")
  )

  # linestring
  expect_identical(coords_linestring_translate_wkt(double(), double()), character(0))
  expect_identical(
    coords_linestring_translate_wkt(1:3, 2:4),
    "LINESTRING (1 2, 2 3, 3 4)"
  )
  expect_identical(
    coords_linestring_translate_wkt(1:3, 2:4, 3:5),
    "LINESTRING Z (1 2 3, 2 3 4, 3 4 5)"
  )
  expect_identical(
    coords_linestring_translate_wkt(1:3, 2:4, NA, 3:5),
    "LINESTRING M (1 2 3, 2 3 4, 3 4 5)"
  )
  expect_identical(
    coords_linestring_translate_wkt(1:3, 2:4, 3:5, 4:6),
    "LINESTRING ZM (1 2 3 4, 2 3 4 5, 3 4 5 6)"
  )

  expect_identical(
    coords_linestring_translate_wkt(1:5, 2:6, feature_id = c(1, 1, 1, 2, 2)),
    c("LINESTRING (1 2, 2 3, 3 4)", "LINESTRING (4 5, 5 6)")
  )

})

test_that("coords_*_translate_wkb() works", {
  expect_identical(
    coords_point_translate_wkb(1:3, 2:4),
    wkt_translate_wkb(c("POINT (1 2)", "POINT (2 3)", "POINT (3 4)"))
  )
  expect_identical(
    coords_linestring_translate_wkb(1:5, 2:6, feature_id = c(1, 1, 1, 2, 2)),
    wkt_translate_wkb(c("LINESTRING (1 2, 2 3, 3 4)", "LINESTRING (4 5, 5 6)"))
  )
})

test_that("coords_*_translate_wksxp() works", {
  expect_identical(
    coords_point_translate_wksxp(1:3, 2:4),
    wkt_translate_wksxp(c("POINT (1 2)", "POINT (2 3)", "POINT (3 4)"))
  )
  expect_identical(
    coords_linestring_translate_wksxp(1:5, 2:6, feature_id = c(1, 1, 1, 2, 2)),
    wkt_translate_wksxp(c("LINESTRING (1 2, 2 3, 3 4)", "LINESTRING (4 5, 5 6)"))
  )
})
