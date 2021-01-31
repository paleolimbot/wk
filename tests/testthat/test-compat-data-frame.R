
test_that("wk_handle() works for data.frame", {
  expect_error(wk_handle(data.frame(a = 1)), "must have exactly one")
  expect_identical(
    wk_handle(data.frame(a = wkt("POINT (0 1)")), wkb_writer()),
    wk_handle(wkt("POINT (0 1)"), wkb_writer())
  )
})

test_that("wk_translate() works for data.frame", {
  expect_identical(
    wk_translate(as_wkb("POINT (1 2)"), data.frame(a = wkt())),
    data.frame(a = wkt("POINT (1 2)"))
  )

  expect_identical(
    wk_translate(
      data.frame(a = as_wkb("POINT (1 2)")),
      data.frame(a = wkt())
    ),
    data.frame(a = wkt("POINT (1 2)"))
  )
})
