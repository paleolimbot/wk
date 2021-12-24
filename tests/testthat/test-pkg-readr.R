
test_that("readr support for writing columns works", {
  skip_if_not_installed("readr")

  x_vctr <- as_wkb(c("POINT (1 2)", "POINT Z (3 4 5)", NA))
  expect_identical(
    readr::output_column(x_vctr),
    c("POINT (1 2)", "POINT Z (3 4 5)", NA)
  )

  expect_identical(
    readr::output_column(xy(c(1, 3, NA), c(2, 4, NA))),
    c("POINT (1 2)", "POINT (3 4)", NA)
  )

  tf <- tempfile()
  readr::write_csv(data.frame(x_vctr = x_vctr), tf)
  expect_identical(
    as.data.frame(readr::read_csv(tf, show_col_types = FALSE)),
    data.frame(x_vctr = c("POINT (1 2)", "POINT Z (3 4 5)", NA))
  )
  unlink(tf)
})
