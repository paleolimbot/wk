
#' Validate well-known binary and well-known text
#'
#' The problems handler returns a character vector of parse
#' errors and can be used to validate input of any type
#' for which [wk_handle()] is defined.
#'
#' @inheritParams wk_handle
#'
#' @return A character vector of parsing errors. `NA` signifies
#'   that there was no parsing error.
#' @export
#'
#' @examples
#' wk_problems(new_wk_wkt(c("POINT EMTPY", "POINT (20 30)")))
#' wk_handle(
#'   new_wk_wkt(c("POINT EMTPY", "POINT (20 30)")),
#'   wk_problems_handler()
#' )
#'
wk_problems <- function(handleable, ...) {
  wk_handle(handleable, wk_problems_handler(), ...)
}

#' @rdname wk_problems
#' @export
wk_problems_handler <- function() {
  new_wk_handler(.Call(wk_c_problems_handler_new), "wk_problems_handler")
}
