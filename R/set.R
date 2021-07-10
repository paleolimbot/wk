
wk_set_z <- function(handleable, z) {
  wk_set_base(wk_trans_set(xyz(NA, NA, z), use_z = TRUE))
}

wk_set_m <- function(handleable, m) {
  wk_set_base(wk_trans_set(xym(NA, NA, m), use_m = TRUE))
}

wk_drop_z <- function(handleable) {
  wk_set_base(wk_trans_set(xy(NA, NA), use_z = FALSE))
}

wk_drop_m <- function(handleable) {
  wk_set_base(wk_trans_set(xy(NA, NA), use_m = FALSE))
}

wk_trans_set <- function(value, use_z = NA, use_m = NA) {
  value <- as_xy(value)
  value <- as_xy(value, dims = c("x", "y", "z", "m"))
  new_wk_trans(
    .Call(wk_c_trans_set_new, value, as.logical(use_z)[1], as.logical(use_m)[1]),
    "wk_trans_set"
  )
}

wk_set_base <- function(handleable, trans) {
  result <- wk_handle(handleable, wk_transform_filter(wk_writer(handleable), trans))
  wk_set_crs(wk_restore(handleable, result), wk_crs(handleable))
}
