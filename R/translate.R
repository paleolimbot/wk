
#' Translate between WKB and WKT
#'
#' @param wkb A `list()` of [raw()] vectors, such as that
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
#' @return [wk_translate_wkb_wkt()] returns a character vector of
#'   well-known text; [wk_translate_wkb_wkb()] returns a list
#'   of raw vectors.
#' @export
#'
wk_translate_wkb_wkt <- function(wkb, precision = 16, trim = TRUE) {
  cpp_translate_wkb_wkt(wkb, precision = precision, trim = trim)
}

#' @rdname wk_translate_wkb_wkt
#' @export
wk_translate_wkb_wkb <- function(wkb, endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_translate_wkb_wkb(wkb, endian = endian, bufferSize = buffer_size)
}

#' @rdname wk_translate_wkb_wkt
#' @export
wk_platform_endian <- function() {
  match(.Platform$endian, c("big", "little")) - 1L
}
