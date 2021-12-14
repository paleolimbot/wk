
#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(
    wk_c_read_wkt,
    list(
      handleable,
      4096L,
      TRUE
    ),
    handler
  )
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
  .Call(
    wk_c_read_wkt,
    list(
      handleable,
      as.integer(wkt_parser_buffer_size)[1],
      FALSE
    ),
    handler
  )
}
