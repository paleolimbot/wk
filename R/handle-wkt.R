
#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(handleable, handler, ..., wkt_parser_buffer_size = 4096) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler, wkt_parser_buffer_size, reveal_size = TRUE)
}

#' Test handlers for handling of unknown size vectors
#'
#' @inheritParams wk_handle
#' @param wkt_parser_buffer_size Buffer size to use for the parser. Useful
#'   for testing the parser.
#' @export
#'
#' @examples
#' handle_wkt_without_vector_size(wkt(), wk_vector_meta_handler())
#'
handle_wkt_without_vector_size <- function(handleable, handler, wkt_parser_buffer_size = 4096) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler, wkt_parser_buffer_size, reveal_size = FALSE)
}
