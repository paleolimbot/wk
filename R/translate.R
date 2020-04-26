
#' Translate between WKB and WKT
#'
#' @param x A `list()` of [raw()] vectors, such as that
#'   returned by [sf::st_as_binary()].
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#'
#' @return A character vector of well-known text.
#' @export
#'
translate_wkb_wkt <- function(x, precision = 16, trim = TRUE) {
  cpp_translate_wkb_wkt(x, precision, trim)
}

#' @rdname translate_wkb_wkt
#' @export
translate_wkb_wkb <- function(x, endian = 1) {
  cpp_translate_wkb_wkb(x, endian)
}
