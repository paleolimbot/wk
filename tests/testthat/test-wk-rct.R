
test_that("rct class works", {
  expect_is(rct(), "wk_rct")
  expect_output(print(rct(1, 2, 3, 4)), "\\[1 2 3 4\\]")

  expect_identical(
    as_rct(as.matrix(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4))),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(data.frame(xmin = 1, ymin = 2, xmax = 3, ymax = 4)),
    rct(1, 2, 3, 4)
  )
  expect_identical(
    as_rct(matrix(1:4, nrow = 1)),
    rct(1, 2, 3, 4)
  )
})
