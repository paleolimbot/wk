test_that("wk_trans_explicit works", {
  expect_identical(
    wk::wk_transform(rep(wk::xy(0, 0), 5), wk_trans_explicit(wk::xy(1:5, 1:5))),
    wk::xy(1:5, 1:5)
  )

  # check with ZM values
  expect_identical(
    wk::wk_transform(
      rep(wk::xyzm(0, 0, 0, 0), 5),
      wk_trans_explicit(wk::xy(1:5, 1:5))
    ),
    wk::xyzm(1:5, 1:5, 0, 0)
  )

  expect_identical(
    wk::wk_transform(
      rep(wk::xyzm(0, 0, 0, 0), 5),
      wk_trans_explicit(wk::xyzm(1:5, 1:5, 1:5, 1:5))
    ),
    wk::xyzm(1:5, 1:5, 1:5, 1:5)
  )

  geoms <- wkt(
    c(
      "POLYGON ((0 0, 0 1, 1 0, 0 0))",
      "POINT (0 0)",
      "POINT Z (0 0 0)",
      "POINT M (0 0 0)",
      "POINT ZM (0 0 0 0)",
      NA
    )
  )

  # explicit transformation
  cds <- wk_coords(geoms)[c("x", "y", "z", "m")]
  cds$x <- cds$x * 2
  expect_identical(
    wk_transform(geoms, wk_trans_explicit(cds)),
    wk_transform(geoms, wk_affine_scale(scale_x = 2))
  )


  # explicit transformation via replacement function
  geoms2 <- geoms
  wk_coords(geoms2) <- cds[c("x", "y", "m")]
  expect_identical(
    geoms2,
    wk_transform(geoms, wk_affine_scale(scale_x = 2))
  )

  wk_coords(geoms2, use_z = FALSE) <- wk_vertices(geoms)
  expect_identical(
    wk::wk_drop_z(geoms),
    geoms2
  )
  geoms3 <- geoms
  wk_coords(geoms3, use_m = FALSE) <- wk_vertices(geoms)
  expect_identical(
    wk::wk_drop_m(geoms),
    geoms3
  )

  # check that crs is dropped when we clobber the coords
  pt <- wkt("POINT (0 1)", crs = "EPSG:3976")
  expect_equal(
    wk_crs(pt),
    "EPSG:3976"
  )
  wk_coords(pt) <- xy(1, 2)
  expect_null(
    wk_crs(pt)
  )
})
