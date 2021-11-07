
test_that("recycle_common works", {
  expect_identical(recycle_common(1, 2), list(1, 2))
  expect_identical(recycle_common(1, b = 2), list(1, b = 2))
  expect_identical(recycle_common(1, 2:4), list(c(1, 1, 1), c(2L, 3L, 4L)))
  expect_identical(recycle_common(numeric(0), 2), list(numeric(0), numeric(0)))
  expect_error(recycle_common(numeric(0), 2:4), "Incompatible lengths")
})

test_that("is_vector_class works", {
  expect_true(is_vector_class(1:5))
  expect_true(is_vector_class(xy(1:5, 1:5)))
  expect_false(is_vector_class(structure(list(), class = "fish")))
})
