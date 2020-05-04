
test_that("wkb_coords() works", {
  wkb <- wkt_translate_wkb("POINT (30 10)")

  expect_identical(
    wkb_coords(wkb),
    data.frame(
      feature_id = 1L,
      nest_id = 0L,
      part_id = NA_integer_,
      ring_id = NA_integer_,
      coord_id = 1L,
      x = 30,
      y = 10,
      z = NA_real_,
      m = NA_real_
    )
  )
})
