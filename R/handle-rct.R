
#' @rdname wk_handle
#' @export
wk_handle.wk_rct <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_rct, handleable, handler)
}
