
test_that("basic WKL translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wkl("POINT (30 10)"),
    list(
      structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wkl("LINESTRING (30 10, 0 0)"),
    list(
      structure(matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE), class = "wk_linestring")
    )
  )
  expect_identical(
    wkt_translate_wkl("POLYGON ((30 10, 0 0, 10 10, 30 10))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 0, 0, 10, 10, 30, 10), ncol = 2, byrow = TRUE)
        ),
        class = "wk_polygon"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl("MULTIPOINT ((30 10), (0 0))"),
    list(
      structure(
        list(
          matrix(c(30, 10), ncol = 2),
          matrix(c(0, 0), ncol = 2)
        ),
        class = "wk_multipoint"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl("MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE),
          matrix(c(20, 20, 0, 0), ncol = 2, byrow = TRUE)
        ),
        class = "wk_multilinestring"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl("MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"),
    list(
      structure(
        list(
          list(
            matrix(c(30, 10, 0, 0, 10, 10, 30, 10), ncol = 2, byrow = TRUE)
          ),
          list(
            matrix(c(30, 10, 0, 0, 10, 10, 30, 10), ncol = 2, byrow = TRUE)
          )
        ),
        class = "wk_multipolygon"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl(
      "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
    ),
    list(
      structure(
        list(
          structure(matrix(c(30, 10), ncol = 2), class = "wk_point"),
          structure(
            list(
              structure(
                matrix(c(12, 6), ncol = 2),
                class = "wk_point"
              )
            ),
            class = "wk_geometrycollection"
          ),
          structure(
            matrix(c(1, 2, 3, 4), ncol = 2, byrow = TRUE),
            class = "wk_linestring"
          )
        ),
        class = "wk_geometrycollection"
      )
    )
  )
})

test_that("basic WKL translation works on non-empty 3D geoms", {
  expect_identical(
    wkt_translate_wkl("POINT Z (30 10 2)"),
    list(
      structure(matrix(c(30, 10, 2),  ncol = 3), has_z = TRUE, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wkl("LINESTRING Z (30 10 3, 0 0 1)"),
    list(
      structure(matrix(c(30, 10, 3, 0, 0, 1), ncol = 3, byrow = TRUE), has_z = TRUE, class = "wk_linestring")
    )
  )
  expect_identical(
    wkt_translate_wkl("POLYGON Z ((30 10 2, 0 0 1, 10 10 2, 30 10 13))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 2, 0, 0, 1, 10, 10, 2, 30, 10, 13), ncol = 3, byrow = TRUE)
        ),
        has_z = TRUE,
        class = "wk_polygon"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl("MULTIPOINT Z ((30 10 5), (0 0 1))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 5), ncol = 3),
          matrix(c(0, 0, 1), ncol = 3)
        ),
        has_z = TRUE,
        class = "wk_multipoint"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl("MULTILINESTRING Z ((30 10 1, 0 0 3), (20 20 9, 0 0 19))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 1, 0, 0, 3), ncol = 3, byrow = TRUE),
          matrix(c(20, 20, 9, 0, 0, 19), ncol = 3, byrow = TRUE)
        ),
        has_z = TRUE,
        class = "wk_multilinestring"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl(
      "MULTIPOLYGON Z (((30 10 1, 0 0 9, 10 10 12, 30 10 9)), ((30 10 9, 0 0 8, 10 10 1, 30 10 -1)))"
    ),
    list(
      structure(
        list(
          list(
            matrix(c(30, 10, 1, 0, 0, 9, 10, 10, 12, 30, 10, 9), ncol = 3, byrow = TRUE)
          ),
          list(
            matrix(c(30, 10, 9, 0, 0, 8, 10, 10, 1, 30, 10, -1), ncol = 3, byrow = TRUE)
          )
        ),
        has_z = TRUE,
        class = "wk_multipolygon"
      )
    )
  )
  expect_identical(
    wkt_translate_wkl(
      "GEOMETRYCOLLECTION (
        POINT Z (30 10 99),
        GEOMETRYCOLLECTION (POINT Z (12 6 10)),
        LINESTRING Z (1 2 3, 3 4 5)
      )"
    ),
    list(
      structure(
        list(
          structure(matrix(c(30, 10, 99), ncol = 3), has_z = TRUE, class = "wk_point"),
          structure(
            list(
              structure(
                matrix(c(12, 6, 10), ncol = 3),
                has_z = TRUE,
                class = "wk_point"
              )
            ),
            class = "wk_geometrycollection"
          ),
          structure(
            matrix(c(1, 2, 3, 3, 4, 5), ncol = 3, byrow = TRUE),
            has_z = TRUE,
            class = "wk_linestring"
          )
        ),
        class = "wk_geometrycollection"
      )
    )
  )
})
