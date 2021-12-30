
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
#' @param endian Force the endian of the resulting WKB.
#' @param ... Used to keep backward compatibility with previous
#'   versions of these functions.
#'
#' @rdname deprecated
#'
wkb_translate_wkt <- function(wkb, ..., precision = 16, trim = TRUE) {
  unclass(wk_handle.wk_wkb(wkb, wkt_writer(precision, trim)))
}

#' @rdname deprecated
#' @export
wkb_translate_wkb <- function(wkb, ..., endian = NA_integer_) {
  unclass(wk_handle.wk_wkb(wkb, wkb_writer(endian = endian)))
}

#' @rdname deprecated
#' @export
wkt_translate_wkt <- function(wkt, ..., precision = 16, trim = TRUE) {
  unclass(wk_handle.wk_wkt(wkt, wkt_writer(precision, trim)))
}

#' @rdname deprecated
#' @export
wkt_translate_wkb <- function(wkt, ..., endian = NA_integer_) {
  unclass(wk_handle.wk_wkt(wkt, wkb_writer(endian = endian)))
}
