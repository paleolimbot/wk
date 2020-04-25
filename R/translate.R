
#' Translate between WKB and WKT
#'
#' @param x A `list()` of [raw()] vectors, such as that
#'   returned by [sf::st_as_binary()].
#'
#' @return A character vector of well-known text.
#' @export
#'
translate_wkb_wkt <- function(x) {
  cpp_translate_wkb_wkt(x)
}
