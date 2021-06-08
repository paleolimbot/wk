
#' Extract vertices
#'
#' @inheritParams wk_handle
#' @param add_details Use `TRUE` to add a "details" attribute, which
#'   contains columns `feature_id`, `part_id`, and `ring_id`.
#'
#' @return The result of the `handler`.
#' @export
#'
#' @examples
#' wk_vertices(wkt("LINESTRING (0 0, 1 1)"))
#'
wk_vertices <- function(handleable, ...) {
  result <- wk_handle(handleable, wk_vertex_filter(wk_writer(handleable)), ...)
  result <- wk_restore(handleable, result, ...)
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_vertices
#' @export
wk_vertex_filter <- function(handler, add_details = FALSE) {
  new_wk_handler(
    .Call("wk_c_vertex_filter_new", as_wk_handler(handler), as.logical(add_details)[1]),
    "wk_vertex_filter"
  )
}
