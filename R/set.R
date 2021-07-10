
#' Set coordinate values
#'
#' @inheritParams wk_handle
#' @param z,m A vector of Z or M values applied feature-wise and recycled
#'    along `handleable`. Use `NA` to keep the existing value of a given
#'    feature.
#' @param value An [xy()], [xyz()], [xym()], or [xyzm()] of coordinates
#'   used to replace values in the input. Use `NA` to keep the existing
#'   value.
#' @param use_z,use_m Used to declare the output type. Use `TRUE` to
#'   ensure the output has that dimension, `FALSE` to ensure it does not,
#'   and `NA` to leave the dimension unchanged.
#' @param
#'
#' @export
#'
#' @examples
#' wk_set_z(wkt("POINT (0 1)"), 2)
#' wk_set_m(wkt("POINT (0 1)"), 2)
#' wk_drop_z(wkt("POINT ZM (0 1 2 3)"))
#' wk_drop_m(wkt("POINT ZM (0 1 2 3)"))
#'
wk_set_z <- function(handleable, z, ...) {
  wk_set_base(handleable, wk_trans_set(xyz(NA, NA, z), use_z = TRUE), ...)
}

#' @rdname wk_set_z
#' @export
wk_set_m <- function(handleable, m, ...) {
  wk_set_base(handleable, wk_trans_set(xym(NA, NA, m), use_m = TRUE), ...)
}

#' @rdname wk_set_z
#' @export
wk_drop_z <- function(handleable, ...) {
  wk_set_base(handleable, wk_trans_set(xy(NA, NA), use_z = FALSE), ...)
}

#' @rdname wk_set_z
#' @export
wk_drop_m <- function(handleable, ...) {
  wk_set_base(handleable, wk_trans_set(xy(NA, NA), use_m = FALSE), ...)
}

#' @rdname wk_set_z
#' @export
wk_trans_set <- function(value, use_z = NA, use_m = NA) {
  value <- as_xy(value)
  value <- as_xy(value, dims = c("x", "y", "z", "m"))
  new_wk_trans(
    .Call(wk_c_trans_set_new, value, as.logical(use_z)[1], as.logical(use_m)[1]),
    "wk_trans_set"
  )
}

wk_set_base <- function(handleable, trans, ...) {
  result <- wk_handle(handleable, wk_transform_filter(wk_writer(handleable), trans), ...)
  wk_set_crs(wk_restore(handleable, result), wk_crs(handleable))
}
