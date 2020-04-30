
#' Validate well-known binary and well-known text
#'
#' @inheritParams wkb_translate_wkt
#'
#' @return A character vector of parsing errors. `NA` signifies
#'   that there was no parsing error.
#' @export
#'
#' @examples
#' # well-known text
#' wkt_problems(c("POINT EMTPY", "POINT (20 30)"))
#'
#' # well-known binary
#' wkb <- wkt_translate_wkb("POINT (30 10)", endian = 1)[[1]]
#' wkb_bad <- wkb
#' wkb_bad[2] <- as.raw(255)
#' wkb_problems(list(wkb, wkb_bad))
#'
wkb_problems <- function(wkb) {
  cpp_problems_wkb(wkb)
}

#' @rdname wkb_problems
#' @export
wkt_problems <- function(wkt) {
  cpp_problems_wkt(wkt)
}
