
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
    wkt(c(paste0("MULTIPOINT (", paste(1:1025, 1, collapse = ", ") , ")"), "POINT (0 0)")),
    wk_flatten_filter(xy_writer(), add_details = TRUE)
  )
  expect_identical(
    attr(xy_copy, "wk_details"),
    list(feature_id = c(rep(1L, 1025), 2L))
  )
  attr(xy_copy, "wk_details") <- NULL
  expect_identical(xy_copy, c(xy(1:1025, 1), xy(0, 0)))
})

test_that("wk_flatten() works for data.frame", {
  expect_equal(
    wk_flatten(data.frame(geom = wkt(c("MULTIPOINT (0 0, 1 1)")))),
    data.frame(geom = wkt(c("POINT (0 0)", "POINT (1 1)"))),
    ignore_attr = TRUE
  )
})
