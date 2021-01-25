
#' Debug filters and handlers
#'
#' @inheritParams wk_handle
#'
#' @return The result of the `handler`.
#' @export
#'
#' @examples
#' wk_debug(wkt("POINT (1 1)"))
#' wk_handle(wkt("POINT (1 1)"), wk_debug_filter())
#'
wk_debug <- function(handleable, handler = wk_void_handler(), ...) {
  wk_handle(handleable, wk_debug_filter(handler))
}

#' @rdname wk_debug
#' @export
wk_debug_filter <- function(handler = wk_void_handler()) {
  new_wk_handler(.Call(wk_c_debug_filter_new, handler), "wk_debug_filter")
}
