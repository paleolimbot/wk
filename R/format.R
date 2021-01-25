
#' Format well-known geometry for printing
#'
#' Provides an abbreviated version of the well-known text
#' representation of a geometry. This returns a constant
#' number of coordinates for each geometry, so is safe to
#' use for geometry vectors with many (potentially large)
#' features. Parse errors are passed on to the format string
#' and do not cause this handler to error.
#'
#' @inheritParams wk_handle
#' @inheritParams wk_writer
#' @param max_coords The maximum number of coordinates to include
#'   in the output.
#'
#' @return A character vector of abbreviated well-known text.
#' @export
#'
#' @examples
#' wk_format(wkt("MULTIPOLYGON (((0 0, 10 0, 0 10, 0 0)))"))
#' wk_format(new_wk_wkt("POINT ENTPY"))
#' wk_handle(
#'   wkt("MULTIPOLYGON (((0 0, 10 0, 0 10, 0 0)))"),
#'   wkt_format_handler()
#' )
#'
wk_format <- function(handleable, precision = 7, trim = TRUE, max_coords = 6, ...) {
  wk_handle(
    handleable,
    wkt_format_handler(precision = precision, trim = trim, max_coords = max_coords),
    ...
  )
}

#' @rdname wk_format
#' @export
wkt_format_handler <- function(precision = 7, trim = TRUE, max_coords = 6) {
  new_wk_handler(wk_cpp_wkt_formatter(precision, trim, max_coords), "wk_wkt_writer")
}
