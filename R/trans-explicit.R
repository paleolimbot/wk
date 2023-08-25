
#' Transform using explicit coordinate values
#'
#' A [wk_trans][wk::wk_transform] implementation that replaces coordinate values
#' using a vector of pre-calculated coordinates. This is used to perform generic
#' transforms using R functions and system calls that are impossible or impractical
#' to implement at the C level.
#'
#' @inheritParams wk_trans_set
#'
#' @export
#'
#' @seealso [wk_coords()] which has a replacement version  "`wk_coords<-`"
#' @examples
#' trans <- wk_trans_explicit(xy(1:5, 1:5))
#' wk_transform(rep(xy(0, 0), 5), trans)
wk_trans_explicit <- function(value, use_z = NA, use_m = NA) {
  value <- wk::as_xy(value)
  value <- wk::as_xy(value, dims = c("x", "y", "z", "m"))
  wk::new_wk_trans(
    .Call(wk_c_trans_explicit_new, value, as.logical(use_z)[1], as.logical(use_m)[1]),
    "wk_trans_explicit"
  )
}
