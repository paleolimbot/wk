
test_that("wk_rcrd works", {
  xy_rcrd <- structure(list(x = as.numeric(1:3), y = c(2, 2, 2)), class = "wk_rcrd")
  expect_identical(length(xy_rcrd), 3L)
  expect_identical(
    xy_rcrd[2],
    structure(list(x = 2, y = 2), class = "wk_rcrd")
  )
  expect_identical(xy_rcrd[[2]], xy_rcrd[2])
  expect_error(xy_rcrd$x, "is not meaningful")

  expect_identical(names(xy_rcrd), NULL)
  expect_identical(is.na(xy_rcrd), c(FALSE, FALSE, FALSE))
  expect_identical(is.na(xy_rcrd[NA_integer_]), TRUE)
  expect_identical(is.na(xy_rcrd[integer(0)]), logical(0))

  expect_identical(expect_output(print(xy_rcrd), "wk_rcrd"), xy_rcrd)
  expect_output(print(xy_rcrd[integer(0)]), "wk_rcrd")
  expect_length(format(xy_rcrd), 2)
  expect_length(as.character(xy_rcrd), 2)

  xy_rcrd2 <- xy_rcrd
  names(xy_rcrd2) <- NULL
  expect_identical(xy_rcrd2, xy_rcrd)
  expect_error(names(xy_rcrd) <- "not null", "must be NULL")
  expect_identical(validate_wk_rcrd(xy_rcrd), xy_rcrd)

  expect_identical(
    rep(xy_rcrd, 2),
    structure(list(x = as.numeric(c(1:3, 1:3)), y = rep(2, 6)), class = "wk_rcrd")
  )
  expect_identical(
    rep_len(xy_rcrd, 6),
    structure(list(x = as.numeric(c(1:3, 1:3)), y = rep(2, 6)), class = "wk_rcrd")
  )

  expect_identical(
    rep(xy_rcrd, 2),
    c(xy_rcrd, xy_rcrd)
  )

  expect_error(c(xy_rcrd, 2), "Can't combine")

  expect_identical(
    as.matrix(xy_rcrd),
    matrix(c(1, 2, 3, 2, 2, 2), ncol = 2, dimnames = list(NULL, c("x", "y")))
  )

  expect_identical(
    as.data.frame(xy_rcrd),
    data.frame(x = c(1, 2, 3), y = c(2, 2, 2))
  )
})
