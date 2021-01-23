
#' Extract vector- and feature-level meta
#'
#' @param x An object with a [wk_handle()] method.
#'
#' @return A data.frame with columns `geometry_type`, `has_z`,
#'   `has_m`, `srid`, and `size`.
#' @export
#'
#' @examples
#' wk_meta(wkt("POINT (1 2)"))
#' wk_vector_meta(wkt("POINT (1 2)"))
#'
wk_meta <- function(x) {
  UseMethod("wk_meta")
}

#' @export
wk_meta.default <- function(x) {
  wk_handle(x, wk_meta_handler())
}

#' @rdname wk_meta
#' @export
wk_meta_handler <- function() {
  new_wk_handler(.Call(wk_c_meta_handler_new), "wk_meta_handler")
}

#' @rdname wk_meta
#' @export
wk_vector_meta <- function(x) {
  UseMethod("wk_vector_meta")
}

#' @export
wk_vector_meta.default <- function(x) {
  # no concept of srid at the vector level
  wk_handle(x, wk_vector_meta_handler())[-4]
}

#' @rdname wk_meta
#' @export
wk_vector_meta_handler <- function() {
  new_wk_handler(.Call(wk_c_vector_meta_handler_new), "wk_vector_meta_handler")
}
