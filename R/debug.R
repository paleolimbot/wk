
#' Debug well-known geometry
#'
#' Prints the raw calls to the `WKBGeometryHandler()`. Useful for writing
#' custom C++ handlers and debugging read problems.
#'
#' @inheritParams wkb_translate_wkt
#'
#' @export
#'
wkb_debug <- function(wkb) {
  cpp_debug_wkb(wkb)
}

#' @rdname wkb_debug
#' @export
wkt_debug <- function(wkt) {
  cpp_debug_wkt(wkt)
}

#' @rdname wkb_debug
#' @export
wkt_streamer_debug <- function(wkt) {
  cpp_debug_wkt_streamer(wkt)
}
