
#' Apply coordinate transformations
#'
#' @inheritParams wk_handle
#' @param trans An external pointer to a wk_trans object
#'
#' @export
#'
#' @examples
#' wk_transform(xy(0, 0), wk_affine_translate(2, 3))
#'
wk_transform <- function(handleable, trans, ...) {
  result <- wk_handle(
    handleable,
    wk_transform_filter(wk_writer(handleable), trans),
    ...
  )
  wk_restore(handleable, result, ...)
}

wk_transform_filter <- function(handler, trans) {
  new_wk_handler(
    .Call(wk_c_trans_filter_new, as_wk_handler(handler), as_wk_trans(trans)),
    "wk_transform_filter"
  )
}

wk_trans_inverse <- function(trans, ...) {
  UseMethod("wk_trans_inverse")
}

as_wk_trans <- function(x, ...) {
  UseMethod("as_wk_trans")
}

as_wk_trans.wk_trans <- function(x, ...) {
  x
}

new_wk_trans <- function(trans_ptr, subclass = character()) {
  stopifnot(typeof(trans_ptr) == "externalptr")
  structure(trans_ptr, class = union(subclass, "wk_trans"))
}
