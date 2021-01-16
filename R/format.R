
#' Format well-known geometry for printing
#'
#' Provides an abbreviated version of the well-known text
#' representation of a geometry. This returns a constant
#' number of coordinates for each geometry, so is safe to
#' use for geometry vectors with many (potentially large)
#' features.
#'
#' @inheritParams wkb_translate_wkt
#' @param max_coords The maximum number of coordinates to include
#'   in the output.
#'
#' @return A character vector of abbreviated well-known text.
#' @export
#'
#' @examples
#' wkt_format("MULTIPOLYGON (((0 0, 10 0, 0 10, 0 0)))")
#' wkb_format(
#'   wkt_translate_wkb(
#'     "MULTIPOLYGON (((0 0, 10 0, 0 10, 0 0)))"
#'   )
#' )
#'
wkb_format <- function(wkb, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkb(wkb, wkt_format_handler(precision, trim, max_coords))
}

#' @rdname wkb_format
#' @export
wkt_format <- function(wkt, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkt(wkt, wkt_format_handler(precision, trim, max_coords))
}
