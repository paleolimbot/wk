
test_that("wk_rcrd works", {
  xy_rcrd <- structure(list(x = 1:3, y = c(2L, 2L, 2L)), class = "wk_rcrd")
  expect_identical(length(xy_rcrd), 3L)
  expect_identical(
    xy_rcrd[2],
    structure(list(x = 2L, y = 2L), class = "wk_rcrd")
  )
  expect_identical(
    rep(xy_rcrd, 2),
    structure(list(x = c(1:3, 1:3), y = rep(2L, 6)), class = "wk_rcrd")
  )
  expect_identical(
    rep_len(xy_rcrd, 6),
    structure(list(x = c(1:3, 1:3), y = rep(2L, 6)), class = "wk_rcrd")
  )

  expect_identical(
    rep(xy_rcrd, 2),
    c(xy_rcrd, xy_rcrd)
  )

  expect_error(c(xy_rcrd, 2), "Can't combine")
})
