
#' Deprecated functions
#'
#' These functions are deprecated and will be removed in a future version.
#'
#' @param wkb A `list()` of [raw()] vectors, such as that
#'   returned by `sf::st_as_binary()`.
#' @param wkt A character vector containing well-known text.
#' @param max_coords The maximum number of coordinates to include
#'   in the output.
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#' @param ... Used to keep backward compatibility with previous
#'   versions of these functions.
#'
#' @rdname deprecated
#'
wkb_format <- function(wkb, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkb(wkb, wkt_format_handler(precision, trim, max_coords))
}

#' @rdname deprecated
#' @export
wkt_format <- function(wkt, max_coords = 3, precision = 6, trim = TRUE) {
  wk_handle.wk_wkt(wkt, wkt_format_handler(precision, trim, max_coords))
}

#' @rdname deprecated
#' @export
wkb_problems <- function(wkb) {
  wk_handle(new_wk_wkb(unclass(wkb)), wk_problems_handler())
}

#' @rdname deprecated
#' @export
wkt_problems <- function(wkt) {
  wk_handle(new_wk_wkt(unclass(wkt)), wk_problems_handler())
}

#' @rdname deprecated
#' @export
wkb_translate_wkt <- function(wkb, ..., precision = 16, trim = TRUE) {
  unclass(wk_handle.wk_wkb(wkb, wkt_writer(precision, trim)))
}

#' @rdname deprecated
#' @export
wkb_translate_wkb <- function(wkb, ...) {
  unclass(wk_handle.wk_wkb(wkb, wkb_writer()))
}

#' @rdname deprecated
#' @export
wkt_translate_wkt <- function(wkt, ..., precision = 16, trim = TRUE) {
  unclass(wk_handle.wk_wkt(wkt, wkt_writer(precision, trim)))
}

#' @rdname deprecated
#' @export
wkt_translate_wkb <- function(wkt, ...) {
  unclass(wk_handle.wk_wkt(wkt, wkb_writer()))
}
