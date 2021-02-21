
#' @rdname wk_handle
#' @export
wk_handle.sfc <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_sfc, handleable, handler)
}
