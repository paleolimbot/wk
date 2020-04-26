
#' Translate between WKB and WKT
#'
#' @param x A `list()` of [raw()] vectors, such as that
#'   returned by [sf::st_as_binary()].
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#' @param endian For WKB writing, 0 for big endian, 1 for little endinan.
#'   Defaults to [wk_platform_endian()].
#' @param buffer_size For WKB writing, the initial buffer size to use for
#'   each feature, in bytes. This will be extended when needed, but if you
#'   are calling this repeatedly with huge geometries, setting this value
#'   to a larger number may result in less copying.
#'
#' @return A character vector of well-known text.
#' @export
#'
translate_wkb_wkt <- function(x, precision = 16, trim = TRUE) {
  cpp_translate_wkb_wkt(x, precision = precision, trim = trim)
}

#' @rdname translate_wkb_wkt
#' @export
translate_wkb_wkb <- function(x, endian = 1, buffer_size = 2048) {
  cpp_translate_wkb_wkb(x, endian = endian, bufferSize = buffer_size)
}

#' @rdname translate_wkb_wkt
#' @export
wk_platform_endian <- function() {
  match(.Platform$endian, c("big", "little")) - 1L
}


