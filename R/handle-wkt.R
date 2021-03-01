
#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler, reveal_size = TRUE)
}
