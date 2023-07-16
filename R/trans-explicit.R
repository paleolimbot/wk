#' Transform using explicit coordinate values
#'
#' A [wk_trans][wk::wk_transform] implementation that replaces coordinate values
#' using a vector of pre-calculated coordinates. This is used to perform generic
#' transforms using R functions and system calls that are impossible or impractical
#' to implement at the C level.
#'
#' @param value A vector of coordinates as a [wk::xy()] used to replace each
#'   coordinate in the input in the order it is encountered.
#' @inheritParams wk_trans_set
#'
#' @export
#'
#' @examples
#' trans <- wk_trans_explicit(wk::xy(1:5, 1:5))
#' wk::wk_transform(rep(wk::xy(0, 0), 5), trans)
#'
wk_trans_explicit <- function(value, use_z = NA, use_m = NA) {
  value <- wk::as_xy(value)
  value <- wk::as_xy(value, dims = c("x", "y", "z", "m"))
  wk::new_wk_trans(
    .Call(wk_c_trans_explicit_new, value, as.logical(use_z)[1], as.logical(use_m)[1]),
    "wk_trans_explicit"
  )
}
