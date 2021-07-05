
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

#' @rdname wk_transform
#' @export
wk_transform_filter <- function(handler, trans) {
  new_wk_handler(
    .Call(wk_c_trans_filter_new, as_wk_handler(handler), as_wk_trans(trans)),
    "wk_transform_filter"
  )
}

#' Generic transform class
#'
#' @param ... Passed to S3 methods
#' @param trans_ptr An external pointer to a wk_trans_t transform
#'   struct.
#' @param subclass An optional subclass to apply to the pointer
#' @inheritParams wk_transform
#'
#' @export
#'
wk_trans_inverse <- function(trans, ...) {
  UseMethod("wk_trans_inverse")
}

#' @rdname wk_transform
#' @export
as_wk_trans <- function(x, ...) {
  UseMethod("as_wk_trans")
}

#' @rdname wk_transform
#' @export
as_wk_trans.wk_trans <- function(x, ...) {
  x
}

#' @rdname wk_transform
#' @export
new_wk_trans <- function(trans_ptr, subclass = character()) {
  stopifnot(typeof(trans_ptr) == "externalptr")
  structure(trans_ptr, class = union(subclass, "wk_trans"))
}
