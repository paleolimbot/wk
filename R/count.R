
#' Count geometry components
#'
#' Counts the number of geometries, rings, and coordinates found within
#' each feature. As opposed to [wk_meta()], this handler will iterate
#' over the entire geometry.
#'
#' @inheritParams wk_handle
#'
#' @return A data.frame with one row for every feature encountered and
#'   columns:
#'   - `n_geom`: The number of geometries encountered, including the
#'     root geometry. Will be zero for a null feature.
#'   - `n_ring`: The number of rings encountered. Will be zero for a
#'     null feature.
#'   - `n_coord`: The number of coordinates encountered. Will be zero
#'     for a null feature.
#' @export
#'
#' @examples
#' wk_count(as_wkt("LINESTRING (0 0, 1 1)"))
#' wk_count(as_wkb("LINESTRING (0 0, 1 1)"))
#'
wk_count <- function(handleable, ...) {
  UseMethod("wk_count")
}

#' @rdname wk_count
#' @export
wk_count.default <- function(handleable, ...) {
  new_data_frame(wk_handle(handleable, wk_count_handler(), ...))
}

#' @rdname wk_count
#' @export
wk_count_handler <- function() {
  new_wk_handler(.Call(wk_c_count_handler_new), "wk_count_handler")
}
