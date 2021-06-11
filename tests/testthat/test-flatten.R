
test_that("wk_flatten() works", {
  expect_identical(
    wk_flatten(wkt(c("MULTIPOINT (0 0, 1 1)", NA))),
    wkt(c("POINT (0 0)", "POINT (1 1)", NA))
  )
  expect_identical(
    wk_vertices(wkt(c("POINT (0 0)", "POINT (1 1)", NA))),
    wkt(c("POINT (0 0)", "POINT (1 1)", NA))
  )
  expect_error(wk_vertices(new_wk_wkt("POINT ENTPY")), "ENTPY")

  # we need this one to trigger a realloc on the details list
  xy_copy <- wk_handle(
    wkt(paste0("MULTIPOINT (", paste(1:1025, 1, collapse = ", ") , ")")),
    wk_flatten_filter(xy_writer(), add_details = TRUE)
  )
  # expect_identical(
  #   attr(xy_copy, "wk_details"),
  #   list(feature_id = 1:1025)
  # )
  attr(xy_copy, "wk_details") <- NULL
  expect_identical(xy_copy, xy(1:1025, 1))
})
