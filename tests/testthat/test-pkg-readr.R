
test_that("readr methods work for all vector types", {
  skip_if_not_installed("readr")

  vctr_wkb <- as_wkb(c("POINT (1 2)", "POINT Z (3 4 5)", NA))
  expect_identical(
    readr::output_column(vctr_wkb),
    c("POINT (1 2)", "POINT Z (3 4 5)", NA)
  )

  vctr_wkt <- as_wkt(c("POINT (1 2)", "POINT Z (3 4 5)", NA))
  expect_identical(
    readr::output_column(vctr_wkt),
    c("POINT (1 2)", "POINT Z (3 4 5)", NA)
  )

  vctr_xy <- as_xy(as_wkt(c("POINT (1 2)", "POINT Z (3 4 5)", NA)))
  expect_identical(
    readr::output_column(vctr_xy),
    c("POINT Z (1 2 nan)", "POINT Z (3 4 5)", NA)
  )

  vctr_rct <- rct(1, 2, 3, 4)[c(1, NA)]
  expect_identical(
    readr::output_column(vctr_rct),
    c("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))", NA)
  )
})

test_that("readr can write files from data frames with wk vectors", {
  skip_if_not_installed("readr")

  x_vctr <- as_wkb(c("POINT (1 2)", "POINT Z (3 4 5)", NA))
  tf <- tempfile()
  readr::write_csv(data.frame(x_vctr = x_vctr), tf)
  expect_identical(
    as.data.frame(readr::read_csv(tf, show_col_types = FALSE)),
    data.frame(
      x_vctr = c("POINT (1 2)", "POINT Z (3 4 5)", NA),
      stringsAsFactors = FALSE
    )
  )
  unlink(tf)
})
