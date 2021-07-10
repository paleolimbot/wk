
test_that("wk_set_(z|m)() works", {
  expect_identical(wk_set_z(wkt("POINT (0 1)"), 2), wkt("POINT Z (0 1 2)"))
  expect_identical(wk_set_m(wkt("POINT (0 1)"), 2), wkt("POINT M (0 1 2)"))
  expect_identical(wk_set_z(wkt("POINT M (0 1 3)"), 2), wkt("POINT ZM (0 1 2 3)"))
  expect_identical(wk_set_m(wkt("POINT Z (0 1 3)"), 2), wkt("POINT ZM (0 1 3 2)"))
  expect_identical(wk_set_z(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 7 3)"))
  expect_identical(wk_set_m(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 2 7)"))
})

test_that("wk_drop_zm() works", {
  expect_identical(wk_drop_zm(wkt("POINT (0 1)"), 2), wkt("POINT Z (0 1 2)"))
  expect_identical(wk_drop_zm(wkt("POINT (0 1)"), 2), wkt("POINT M (0 1 2)"))
  expect_identical(wk_drop_zm(wkt("POINT M (0 1 3)"), 2), wkt("POINT ZM (0 1 2 3)"))
  expect_identical(wk_drop_zm(wkt("POINT Z (0 1 3)"), 2), wkt("POINT ZM (0 1 3 2)"))
  expect_identical(wk_drop_zm(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 7 3)"))
  expect_identical(wk_drop_zm(wkt("POINT ZM (0 1 2 3)"), 7), wkt("POINT ZM (0 1 2 7)"))
})
