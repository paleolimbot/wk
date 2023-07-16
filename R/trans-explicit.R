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
#' @seealso [wk_coords] which has a replacement version  "`wk_coords<-`"
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

#' @details
#'
#' 'wk_coords<-' is a replacement-function version of 'wk_coords'. Using the engine of
#' [wk_trans_explicit] the coordinates of an object can be transformed in a generic way.
#'
#' @rdname wk_vertices
#' @aliases "wk_coords<-"
#' @param object handleable to be modified by replacing coordinates with those in `value`
#' @param value handleable whose coordinates will be used to update `object`
#' @inheritParams wk_trans_set
#' @export
#' @examples
#' # wk_coords replacement function
#' x <- xy(1:5, 1:5)
#' y <- as_wkt(x)
#' wk_coords(y) <- cbind(5:1, 0:4)
#' wk_coords(x) <- y[5:1]
"wk_coords<-" <-
  function(object, value, use_z = NA, use_m = NA) {
    wk_transform(object, wk_trans_explicit(value, use_z = use_z, use_m = use_m))
  }
