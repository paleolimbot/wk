
#' Validate well-known binary
#'
#' @inheritParams wk_translate_wkb_wkt
#'
#' @return A character vector of parsing errors. `NA` signifies
#'   that there was no parsing error.
#' @export
#'
wk_problems_wkb <- function(wkb) {
  cpp_problems_wkb(wkb)
}
