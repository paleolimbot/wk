
#' Deprecated functions
#'
#' These functions are deprecated and will be removed in a future version.
#'
#' @inheritParams wkb_translate_wkt
#' @param max_coords The maximum number of coordinates to include
#'   in the output.
#'
wkb_format <- function(wkb, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkb(wkb, wkt_format_handler(precision, trim, max_coords))
}

#' @rdname wkb_format
#' @export
wkt_format <- function(wkt, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkt(wkt, wkt_format_handler(precision, trim, max_coords))
}
