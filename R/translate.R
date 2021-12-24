
#' Translate geometry vectors
#'
#' @inheritParams wk_handle
#' @param to A prototype object.
#'
#' @export
#'
wk_translate <- function(handleable, to, ...) {
  UseMethod("wk_translate", to)
}

#' @rdname wk_translate
#' @export
wk_translate.default <- function(handleable, to, ...) {
  result <- wk_handle(handleable, wk_writer(to), ...)
  attr(result, "crs") <- wk_crs_output(handleable, to)
  wk_set_geodesic(result, wk_is_geodesic(handleable))
}
