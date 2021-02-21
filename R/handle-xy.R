
#' @rdname wk_handle
#' @export
wk_handle.wk_xy <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_xy, handleable, handler)
}
