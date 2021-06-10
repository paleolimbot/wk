
#' Extract vertices
#'
#' These functions provide ways to extract individual coordinate values.
#' Whereas `wk_vertices()` returns a vector of coordinates as in the same
#' format as the input, `wk_coords()` returns a data frame with coordinates
#' as columns.
#'
#' @inheritParams wk_handle
#' @param add_details Use `TRUE` to add a "details" attribute, which
#'   contains columns `feature_id`, `part_id`, and `ring_id`.
#'
#' @return
#'   - `wk_vertices()` extracts vertices and returns the in the same format as
#'     the handler
#'   - `wk_coords()` returns a data frame with columns columns `feature_id`
#'     (the index of the feature from whence it came), `part_id` (an arbitrary
#'     integer identifying the point, line, or polygon from whence it came),
#'     `ring_id` (an arbitrary integer identifying individual rings within
#'     polygons), and one column per coordinate (`x`, `y`, and/or `z` and/or `m`).
#' @export
#'
#' @examples
#' wk_vertices(wkt("LINESTRING (0 0, 1 1)"))
#' wk_coords(wkt("LINESTRING (0 0, 1 1)"))
#'
wk_vertices <- function(handleable, ...) {
  # the results of this handler are not necessarily the same length as the input,
  # so we need to special-case data frames
  if (is.data.frame(handleable))  {
    result <- wk_handle(
      handleable,
      wk_vertex_filter(wk_writer(handleable), add_details = TRUE),
      ...
    )
    feature_id <- attr(result, "details", exact = TRUE)$feature_id
    attr(result, "details") <- NULL
    result <- wk_restore(handleable[feature_id, , drop = FALSE], result, ...)
  } else {
    result <- wk_handle(handleable, wk_vertex_filter(wk_writer(handleable, generic = TRUE)), ...)
    result <- wk_restore(handleable, result, ...)
  }

  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_vertices
#' @export
wk_coords <- function(handleable, ...) {
  result <- wk_handle(
    handleable,
    wk_vertex_filter(xy_writer(), add_details = TRUE),
    ...
  )

  details <- attr(result, "details", exact = TRUE)
  attr(result, "details") <- NULL
  new_data_frame(c(details, unclass(result)))
}

#' @rdname wk_vertices
#' @export
wk_vertex_filter <- function(handler, add_details = FALSE) {
  new_wk_handler(
    .Call("wk_c_vertex_filter_new", as_wk_handler(handler), as.logical(add_details)[1]),
    "wk_vertex_filter"
  )
}
