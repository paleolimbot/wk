
test_that("wkt_draw_* works", {
  x <- "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))"

  expect_identical(wkt_plot_new(x, main  = "wkt_draw_*()"), x)
  expect_identical(wkt_draw_polypath(x, col = "grey90"), x)
  expect_identical(wkt_draw_lines(x, col = "red"), x)
  expect_identical(wkt_draw_points(x, pch = 16), x)
})

test_that("wkb_draw_* works", {
  x <- wkt_translate_wkb("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")

  expect_identical(wkb_plot_new(x, main  = "wkb_draw_*()"), x)
  expect_identical(wkb_draw_polypath(x, col = "grey90"), x)
  expect_identical(wkb_draw_lines(x, col = "red"), x)
  expect_identical(wkb_draw_points(x, pch = 16), x)
})

test_that("wksxp_draw_* works", {
  x <- wkt_translate_wksxp("POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))")

  expect_identical(wksxp_plot_new(x, main  = "wksxp_draw_*()"), x)
  expect_identical(wksxp_draw_polypath(x, col = "grey90"), x)
  expect_identical(wksxp_draw_lines(x, col = "red"), x)
  expect_identical(wksxp_draw_points(x, pch = 16), x)
})
