
test_that("plot.wk_wkt works", {
  example_wkt <- c(
    NA,
    "POINT (30 10)",
    "POINT EMPTY",
    "POINT Z (1 1 5)",
    "MULTIPOINT (10 40, 40 30, 20 20, 30 10)",
    "MULTIPOINT EMPTY",
    "POLYGON ((30 10, 40 40, 20 40, 10 20, 30 10))",
    "POLYGON ((35 10, 45 45, 15 40, 10 20, 35 10), (20 30, 35 35, 30 20, 20 30))",
    "POLYGON EMPTY",
    "MULTIPOINT ((10 40), (40 30), (20 20), (30 10))",
    "MULTILINESTRING ((10 10, 20 20, 10 40), (40 40, 30 30, 40 20, 30 10))",
    "MULTILINESTRING EMPTY",
    "MULTIPOLYGON (((30 20, 45 40, 10 40, 30 20)), ((15 5, 40 10, 10 20, 5 10, 15 5)))",
    "MULTIPOLYGON (
      ((40 40, 20 45, 45 30, 40 40)), ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35),
      (30 20, 20 15, 20 25, 30 20))
    )",
    "MULTIPOLYGON EMPTY",
    "GEOMETRYCOLLECTION (
      POINT (40 10),
      LINESTRING (10 10, 20 20, 10 40),
      POLYGON ((40 40, 20 45, 45 30, 40 40))
    )",
    "GEOMETRYCOLLECTION EMPTY"
  )

  plot(as_wkt(example_wkt))
  plot(as_wkb(example_wkt))
  plot(as_wksxp(example_wkt))

  expect_true(TRUE)
})
