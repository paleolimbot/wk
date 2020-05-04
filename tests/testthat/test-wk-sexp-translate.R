
test_that("basic WKL translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wksexp("POINT (30 10)"),
    list(
      structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksexp("LINESTRING (30 10, 0 0)"),
    list(
      structure(matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE), class = "wk_linestring")
    )
  )
  expect_identical(
    wkt_translate_wksexp("POLYGON ((30 10, 0 0, 10 10, 30 10))"),
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
    wkt_translate_wksexp("MULTIPOINT ((30 10), (0 0))"),
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
    wkt_translate_wksexp("MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))"),
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
    wkt_translate_wksexp("MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"),
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
    wkt_translate_wksexp(
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

test_that("basic WKL translation works on non-empty Z geoms", {
  expect_identical(
    wkt_translate_wksexp("POINT Z (30 10 2)"),
    list(
      structure(matrix(c(30, 10, 2),  ncol = 3), has_z = TRUE, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksexp("MULTIPOINT Z ((30 10 5), (0 0 1))"),
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
    wkt_translate_wksexp(
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


test_that("basic WKL translation works on non-empty M geoms", {
  expect_identical(
    wkt_translate_wksexp("POINT M (30 10 2)"),
    list(
      structure(matrix(c(30, 10, 2),  ncol = 3), has_m = TRUE, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksexp("MULTIPOINT M ((30 10 5), (0 0 1))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 5), ncol = 3),
          matrix(c(0, 0, 1), ncol = 3)
        ),
        has_m = TRUE,
        class = "wk_multipoint"
      )
    )
  )
  expect_identical(
    wkt_translate_wksexp(
      "GEOMETRYCOLLECTION (
        POINT M (30 10 99),
        GEOMETRYCOLLECTION (POINT M (12 6 10)),
        LINESTRING M (1 2 3, 3 4 5)
      )"
    ),
    list(
      structure(
        list(
          structure(matrix(c(30, 10, 99), ncol = 3), has_m = TRUE, class = "wk_point"),
          structure(
            list(
              structure(
                matrix(c(12, 6, 10), ncol = 3),
                has_m = TRUE,
                class = "wk_point"
              )
            ),
            class = "wk_geometrycollection"
          ),
          structure(
            matrix(c(1, 2, 3, 3, 4, 5), ncol = 3, byrow = TRUE),
            has_m = TRUE,
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
    wkt_translate_wksexp("POINT ZM (30 10 2 13)"),
    list(
      structure(
        matrix(c(30, 10, 2, 13),  ncol = 4),
        has_z = TRUE, has_m = TRUE, class = "wk_point"
      )
    )
  )

  expect_identical(
    wkt_translate_wksexp("MULTIPOINT ZM ((30 10 5 1), (0 0 1 6))"),
    list(
      structure(
        list(
          matrix(c(30, 10, 5, 1), ncol = 4),
          matrix(c(0, 0, 1, 6), ncol = 4)
        ),
        has_z = TRUE,
        has_m = TRUE,
        class = "wk_multipoint"
      )
    )
  )

  expect_identical(
    wkt_translate_wksexp(
      "GEOMETRYCOLLECTION (
        POINT ZM (30 10 99 2),
        GEOMETRYCOLLECTION (POINT ZM (12 6 10 9)),
        LINESTRING ZM (1 2 3 4, 3 4 5 6)
      )"
    ),
    list(
      structure(
        list(
          structure(matrix(c(30, 10, 99, 2), ncol = 4), has_z = TRUE, has_m = TRUE, class = "wk_point"),
          structure(
            list(
              structure(
                matrix(c(12, 6, 10, 9), ncol = 4),
                has_z = TRUE,
                has_m = TRUE,
                class = "wk_point"
              )
            ),
            class = "wk_geometrycollection"
          ),
          structure(
            matrix(c(1, 2, 3, 4, 3, 4, 5, 6), ncol = 4, byrow = TRUE),
            has_z = TRUE,
            has_m = TRUE,
            class = "wk_linestring"
          )
        ),
        class = "wk_geometrycollection"
      )
    )
  )
})


test_that("basic WKL translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wksexp("SRID=837;POINT (30 10)"),
    list(
      structure(matrix(c(30, 10),  ncol = 2), srid = 837, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksexp("SRID=12;MULTIPOINT ((30 10), (0 0))"),
    list(
      structure(
        list(
          matrix(c(30, 10), ncol = 2),
          matrix(c(0, 0), ncol = 2)
        ),
        srid=12,
        class = "wk_multipoint"
      )
    )
  )
  expect_identical(
    wkt_translate_wksexp(
      "SRID=89;GEOMETRYCOLLECTION (
        POINT (30 10),
        GEOMETRYCOLLECTION (POINT (12 6)),
        LINESTRING (1 2, 3 4)
      )"
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
        srid = 89,
        class = "wk_geometrycollection"
      )
    )
  )
})
