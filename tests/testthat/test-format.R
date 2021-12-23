
test_that("format() works for wkt", {
  expect_identical(
    wk_format(wkt("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wk_format(wkt("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wk_format(wkt(NA_character_)), "<null feature>")
})

test_that("format() works for wkb", {
  expect_identical(
    wk_format(as_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 3),
    "LINESTRING (0 1, 2 3, 4 5..."
  )
  expect_identical(
    wk_format(as_wkb("LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"), max_coords = 10),
    "LINESTRING (0 1, 2 3, 4 5, 6 7, 8 9)"
  )
  expect_identical(wk_format(wkb(list(NULL))), "<null feature>")
})

test_that("format() handles errors", {
  bad_wkb <- unclass(as_wkb("POINT (30 10)", endian = 1L))
  bad_wkb[[1]][2:3] <- as.raw(0xff)
  expect_match(wk_format(new_wk_wkb(bad_wkb)), "!!!")
  expect_match(wk_format(new_wk_wkt("POINT ENTPY")), "!!!")
})

test_that("format handlers return abbreviated WKT", {
  expect_identical(
    wk_handle(
      new_wk_wkt(c(NA, "LINESTRING (0 1, 1 2)", "LINESTRING (0 1, 2 3, 4 5)", "NOT WKT")),
      wkt_format_handler(max_coords = 3)
    ),
    c(
      "<null feature>",
      "LINESTRING (0 1, 1 2)",
      "LINESTRING (0 1, 2 3, 4 5...",
      "!!! Expected geometry type or 'SRID=' but found 'NOT' at byte 0"
    )
  )
})

test_that("wkt_format_handler() works for a vector of indeterminate length", {
  long_xy <- as_wkt(xy(runif(2048), runif(2048)))
  expect_identical(
    handle_wkt_without_vector_size(long_xy, wkt_format_handler()),
    wk_handle(long_xy, wkt_format_handler())
  )
})
