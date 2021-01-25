
#' Do nothing
#'
#' This handler does nothing and returns `NULL`. It is useful for
#' benchmarking readers and handlers and when using filters
#' that have side-effects (e.g., [wk_debug()]).
#'
#' @inheritParams wk_handle
#'
#' @return `NULL`
#' @export
#'
#' @examples
#' wk_void(wkt("POINT (1 4)"))
#' wk_handle(wkt("POINT (1 4)"), wk_void_handler())
#'
wk_void <- function(handleable, ...) {
  invisible(wk_handle(handleable, wk_void_handler(), ...))
}

#' @rdname wk_void
#' @export
wk_void_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_void_new), "wk_void_handler")
}
