
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

test_that("coercion to and from wk* classes works", {
  expect_identical(
    as_wkt(rct(1, 2, 3, 4)),
    wkt("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )

  expect_identical(
    as_wkb(rct(1, 2, 3, 4)),
    as_wkb("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )

  expect_identical(
    as_wksxp(rct(1, 2, 3, 4)),
    as_wksxp("POLYGON ((1 2, 3 2, 3 4, 1 4, 1 2))")
  )
})
