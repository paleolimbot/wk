
#' Create lines, polygons, and collections
#'
#' @inheritParams wk_handle
#' @param feature_id An identifier where changes in sequential
#'   values indicate a new feature.
#' @param ring_id An identifier where changes in sequential
#'   values indicate a new ring. Rings are automatically
#'   closed.
#' @param geometry_type The collection type to create.
#'
#' @return An object of the same class as `handleable` with
#'   whose coordinates have been assembled into the given
#'   type.
#' @export
#'
#' @examples
#' wk_make_linestring(xy(c(1, 1), c(2, 3)))
#' wk_make_polygon(xy(c(0, 1, 1), c(0, 0, 1)))
#' wk_make_collection(xy(c(1, 1), c(2, 3)))
#'
wk_make_linestring <- function(handleable, feature_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(handleable, wk_linestring_filter(writer, feature_id), ...)
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_make_linestring
#' @export
wk_make_polygon <- function(handleable, feature_id = 1L, ring_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(handleable, wk_polygon_filter(writer, feature_id, ring_id), ...)
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_make_linestring
#' @export
wk_make_collection <- function(handleable, geometry_type = 7L, feature_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(handleable, wk_collection_filter(writer, geometry_type, feature_id), ...)
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_make_linestring
#' @export
wk_linestring_filter <- function(handler, feature_id = 1L) {
  .Call(wk_c_linestring_filter_new, handler, feature_id)
}

#' @rdname wk_make_linestring
#' @export
wk_polygon_filter <- function(handler, feature_id = 1L, ring_id = 1L) {
  .Call(wk_c_polygon_filter_new, handler, feature_id)
}

#' @rdname wk_make_linestring
#' @export
wk_collection_filter <- function(handler, feature_id = 1L, geometry_type = 7L) {
  .Call(wk_c_collection_filter_new, handler, feature_id, geometry_type)
}
