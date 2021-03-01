
#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler, reveal_size = TRUE)
}

#' Test handlers for handling of unknown size vectors
#'
#' @inheritParams wk_handle
#' @export
#'
#' @examples
#' handle_wkt_without_vector_size(wkt(), wk_vector_meta_handler())
#'
handle_wkt_without_vector_size <- function(handleable, handler) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler, reveal_size = FALSE)
}
