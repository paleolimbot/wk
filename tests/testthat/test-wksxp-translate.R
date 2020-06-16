
test_that("basic wksxp translation works to WKB", {
  expect_identical(
    wkb_translate_wksxp(wkt_translate_wkb("POINT (30 10)")),
    list(
      structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
    )
  )
})

test_that("basic wksxp translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wksxp("POINT (30 10)"),
    list(
      structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksxp("LINESTRING (30 10, 0 0)"),
    list(
      structure(matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE), class = "wk_linestring")
    )
  )
  expect_identical(
    wkt_translate_wksxp("POLYGON ((30 10, 0 0, 10 10, 30 10))"),
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
    wkt_translate_wksxp("MULTIPOINT ((30 10), (0 0))"),
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
    wkt_translate_wksxp("MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))"),
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
    wkt_translate_wksxp("MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"),
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
    wkt_translate_wksxp(
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

test_that("basic wksxp translation works on non-empty Z geoms", {
  expect_identical(
    wkt_translate_wksxp("POINT Z (30 10 2)"),
    list(
      structure(matrix(c(30, 10, 2),  ncol = 3), has_z = TRUE, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksxp("MULTIPOINT Z ((30 10 5), (0 0 1))"),
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
    wkt_translate_wksxp(
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


test_that("basic wksxp translation works on non-empty M geoms", {
  expect_identical(
    wkt_translate_wksxp("POINT M (30 10 2)"),
    list(
      structure(matrix(c(30, 10, 2),  ncol = 3), has_m = TRUE, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksxp("MULTIPOINT M ((30 10 5), (0 0 1))"),
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
    wkt_translate_wksxp(
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

test_that("basic wksxp translation works on non-empty 3D geoms", {
  expect_identical(
    wkt_translate_wksxp("POINT ZM (30 10 2 13)"),
    list(
      structure(
        matrix(c(30, 10, 2, 13),  ncol = 4),
        has_z = TRUE, has_m = TRUE, class = "wk_point"
      )
    )
  )

  expect_identical(
    wkt_translate_wksxp("MULTIPOINT ZM ((30 10 5 1), (0 0 1 6))"),
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
    wkt_translate_wksxp(
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


test_that("basic wksxp translation works on non-empty 2D geoms", {
  expect_identical(
    wkt_translate_wksxp("SRID=837;POINT (30 10)"),
    list(
      structure(matrix(c(30, 10),  ncol = 2), srid = 837, class = "wk_point")
    )
  )
  expect_identical(
    wkt_translate_wksxp("SRID=12;MULTIPOINT ((30 10), (0 0))"),
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
    wkt_translate_wksxp(
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

test_that("basic reverse wksxp translation works on non-empty 2D geoms", {
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
      )
    ),
    "POINT (30 10)"
  )
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE), class = "wk_linestring")
      )
    ),
    "LINESTRING (30 10, 0 0)"
  )
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10, 0, 0, 10, 10, 30, 10), ncol = 2, byrow = TRUE)
          ),
          class = "wk_polygon"
        )
      )
    ),
    "POLYGON ((30 10, 0 0, 10 10, 30 10))"
  )
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10), ncol = 2),
            matrix(c(0, 0), ncol = 2)
          ),
          class = "wk_multipoint"
        )
      )
    ),
    "MULTIPOINT ((30 10), (0 0))"
  )
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10, 0, 0), ncol = 2, byrow = TRUE),
            matrix(c(20, 20, 0, 0), ncol = 2, byrow = TRUE)
          ),
          class = "wk_multilinestring"
        )
      )
    ),
    "MULTILINESTRING ((30 10, 0 0), (20 20, 0 0))",
  )
  expect_identical(
    wksxp_translate_wkt(
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
    ),
    "MULTIPOLYGON (((30 10, 0 0, 10 10, 30 10)), ((30 10, 0 0, 10 10, 30 10)))"
  )
  expect_identical(
    wksxp_translate_wkt(
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
    ),
    "GEOMETRYCOLLECTION (POINT (30 10), GEOMETRYCOLLECTION (POINT (12 6)), LINESTRING (1 2, 3 4))"
  )
})

test_that("basic reverse wksxp translation works on non-empty ZM geoms", {
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          matrix(c(30, 10, 1, 2),  ncol = 4),
          has_z = TRUE, has_m = TRUE, class = "wk_point"
        )
      )
    ),
    "POINT ZM (30 10 1 2)"
  )

  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10, 1, 2, 0, 0, 1, 2, 10, 10, 1, 2, 30, 10, 1, 2), ncol = 4, byrow = TRUE)
          ),
          has_z = TRUE,
          has_m = TRUE,
          class = "wk_polygon"
        )
      )
    ),
    "POLYGON ZM ((30 10 1 2, 0 0 1 2, 10 10 1 2, 30 10 1 2))"
  )

  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10, 1, 2), ncol = 4),
            matrix(c(0, 0, 1, 2), ncol = 4)
          ),
          has_z = TRUE,
          has_m = TRUE,
          class = "wk_multipoint"
        )
      )
    ),
    "MULTIPOINT ZM ((30 10 1 2), (0 0 1 2))"
  )

  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            matrix(c(30, 10, 1, 2, 0, 0, 1, 2), ncol = 4, byrow = TRUE),
            matrix(c(20, 20, 1, 2, 0, 0, 1, 2), ncol = 4, byrow = TRUE)
          ),
          has_z = TRUE,
          has_m = TRUE,
          class = "wk_multilinestring"
        )
      )
    ),
    "MULTILINESTRING ZM ((30 10 1 2, 0 0 1 2), (20 20 1 2, 0 0 1 2))",
  )

  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          list(
            list(
              matrix(c(30, 10, 1, 2, 0, 0, 1, 2, 10, 10, 1, 2, 30, 10, 1, 2), ncol = 4, byrow = TRUE)
            ),
            list(
              matrix(c(30, 10, 2, 3, 0, 0, 2, 3, 10, 10, 2, 3, 30, 10, 2, 3), ncol = 4, byrow = TRUE)
            )
          ),
          has_z = TRUE,
          has_m = TRUE,
          class = "wk_multipolygon"
        )
      )
    ),
    "MULTIPOLYGON ZM (((30 10 1 2, 0 0 1 2, 10 10 1 2, 30 10 1 2)), ((30 10 2 3, 0 0 2 3, 10 10 2 3, 30 10 2 3)))"
  )
})

test_that("basic reverse wksxp translation works with SRID", {
  expect_identical(
    wksxp_translate_wkt(
      list(
        structure(
          matrix(c(30, 10),  ncol = 2),
          srid = 43, class = "wk_point"
        )
      )
    ),
    "SRID=43;POINT (30 10)"
  )
})

test_that("basic reverse wksxp translation works with NULL", {
  expect_identical(wksxp_translate_wkt(list(NULL)), NA_character_)
})

test_that("identity wksxp translation works", {
  expect_identical(
    wksxp_translate_wksxp(
      list(
        structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
      )
    ),
    list(
      structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
    )
  )
})

test_that("wksxp to wkb works", {
  expect_identical(
    wksxp_translate_wkb(
      list(
        structure(matrix(c(30, 10),  ncol = 2), class = "wk_point")
      )
    ),
    wkt_translate_wkb("POINT (30 10)")
  )
})

test_that("wksxp_translate_* doesn't segfault on other inputs", {
  expect_error(wksxp_translate_wkt("POINT (30 10)"), class = "WKParseException")
  expect_error(wksxp_translate_wkt(as_wkb("POINT (30 10)")), class = "WKParseException")
})
