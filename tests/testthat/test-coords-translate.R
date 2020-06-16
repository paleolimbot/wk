
test_that("coords_*_translate_wkt() works", {
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
})

test_that("coords_*_translate_wkb() works", {
  expect_identical(
    coords_point_translate_wkb(1:3, 2:4),
    wkt_translate_wkb(c("POINT (1 2)", "POINT (2 3)", "POINT (3 4)"))
  )
})

test_that("coords_*_translate_wksxp() works", {
  expect_identical(
    coords_point_translate_wksxp(1:3, 2:4),
    wkt_translate_wksxp(c("POINT (1 2)", "POINT (2 3)", "POINT (3 4)"))
  )
})
