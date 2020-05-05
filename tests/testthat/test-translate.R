
test_that("wksxp writing is vectorized", {
  expect_identical(
    wkt_translate_wksxp(c("POINT (20 20)", "POINT (30 30)")),
    list(
      structure(matrix(c(20, 20),  ncol = 2), class = "wk_point"),
      structure(matrix(c(30, 30),  ncol = 2), class = "wk_point")
    )
  )
})

test_that("wkt writing is vectorized", {
  expect_identical(
    wkt_translate_wkt(c("POINT (20 20)", "POINT (30 30)")),
    c("POINT (20 20)", "POINT (30 30)")
  )
})

test_that("wkb writing is vectorized", {
  expect_identical(
    wkt_translate_wkb(c("POINT (20 20)", "POINT (30 30)")),
    list(
      as.raw(
        c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x34, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x34, 0x40
        )
      ),
      as.raw(
        c(0x01, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, 0x00, 0x00, 0x00, 0x3e, 0x40, 0x00, 0x00, 0x00, 0x00, 0x00,
          0x00, 0x3e, 0x40
        )
      )
    )
  )
})
