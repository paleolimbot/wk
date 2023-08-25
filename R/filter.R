
#' Copy a geometry vector
#'
#' @inheritParams wk_handle
#' @param result The result of a filter operation intended to be a
#'   transformation.
#'
#' @return A copy of `handleable`.
#' @export
#'
#' @examples
#' wk_identity(wkt("POINT (1 2)"))
#'
wk_identity <- function(handleable, ...) {
  result <- wk_handle(handleable, wk_identity_filter(wk_writer(handleable)), ...)
  result <- wk_restore(handleable, result, ...)
  result <- wk_set_crs(result, wk_crs(handleable))
  wk_set_geodesic(result, wk_is_geodesic(handleable))
}

#' @rdname wk_identity
#' @export
wk_identity_filter <- function(handler) {
  new_wk_handler(.Call("wk_c_identity_filter_new", as_wk_handler(handler)), "wk_identity_filter")
}

#' @rdname wk_identity
#' @export
wk_restore <- function(handleable, result, ...) {
  UseMethod("wk_restore")
}

#' @rdname wk_identity
#' @export
wk_restore.default <- function(handleable, result, ...) {
  result
}
