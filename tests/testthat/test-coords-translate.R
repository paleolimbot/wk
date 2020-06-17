
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

  # polygon
  expect_identical(coords_polygon_translate_wkt(double(), double()), character(0))
  expect_identical(
    coords_polygon_translate_wkt(c(0, 10, 0), c(0, 0, 10)),
    "POLYGON ((0 0, 10 0, 0 10, 0 0))"
  )
  expect_identical(
    coords_polygon_translate_wkt(c(0, 10, 0, 0), c(0, 0, 10, 0)),
    "POLYGON ((0 0, 10 0, 0 10, 0 0))"
  )

  expect_identical(
    coords_polygon_translate_wkt(
      c(20, 10, 10, 30, 45, 30, 20, 20),
      c(35, 30, 10, 5, 20, 20, 15, 25),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2)
    ),
    "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))"
  )

  expect_identical(
    coords_polygon_translate_wkt(
      c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
      c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30),
      feature_id = c(rep(1, 8), rep(2, 3)),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    ),
    c(
      "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
      "POLYGON ((40 40, 20 45, 45 30, 40 40))"
    )
  )

  expect_identical(
    coords_polygon_translate_wkt(
      c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
      c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30),
      feature_id = c(rep(1, 8), rep(2, 3)),
      # new ring should be detected on new feature_id
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2)
    ),
    c(
      "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
      "POLYGON ((40 40, 20 45, 45 30, 40 40))"
    )
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
  expect_identical(
    coords_polygon_translate_wkb(
      c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
      c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30),
      feature_id = c(rep(1, 8), rep(2, 3)),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    ),
    wkt_translate_wkb(
      c(
        "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
        "POLYGON ((40 40, 20 45, 45 30, 40 40))"
      )
    )
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
  expect_identical(
    coords_polygon_translate_wksxp(
      c(20, 10, 10, 30, 45, 30, 20, 20, 40, 20, 45),
      c(35, 30, 10, 5, 20, 20, 15, 25, 40, 45, 30),
      feature_id = c(rep(1, 8), rep(2, 3)),
      ring_id = c(1, 1, 1, 1, 1, 2, 2, 2, 1, 1, 1)
    ),
    wkt_translate_wksxp(
      c(
        "POLYGON ((20 35, 10 30, 10 10, 30 5, 45 20, 20 35), (30 20, 20 15, 20 25, 30 20))",
        "POLYGON ((40 40, 20 45, 45 30, 40 40))"
      )
    )
  )
})
