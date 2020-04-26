
#' Validate well-known binary
#'
#' @inheritParams wkb_translate_wkt
#'
#' @return A character vector of parsing errors. `NA` signifies
#'   that there was no parsing error.
#' @export
#'
wkb_problems <- function(wkb) {
  cpp_problems_wkb(wkb)
}
