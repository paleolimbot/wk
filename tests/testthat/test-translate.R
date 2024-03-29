
test_that("wk_translate.wkt works", {
  expect_identical(
    wk_translate(as_wkb("POINT (1 2)"), wkt()),
    wkt("POINT (1 2)")
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
    wkt_translate_wkb(c("POINT (20 20)", "POINT (30 30)"), endian = 1L),
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
