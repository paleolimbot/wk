
#' Create lines, polygons, and collections
#'
#' @inheritParams wk_handle
#' @param feature_id An identifier where changes in sequential
#'   values indicate a new feature. This is recycled silently
#'   as needed.
#' @param ring_id An identifier where changes in sequential
#'   values indicate a new ring. Rings are automatically
#'   closed. This is recycled silently as needed.
#' @param geometry_type The collection type to create.
#'
#' @return An object of the same class as `handleable` with
#'   whose coordinates have been assembled into the given
#'   type.
#' @export
#'
#' @examples
#' wk_linestring(xy(c(1, 1), c(2, 3)))
#' wk_polygon(xy(c(0, 1, 0), c(0, 0, 1)))
#' wk_collection(xy(c(1, 1), c(2, 3)))
#'
wk_linestring <- function(handleable, feature_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(handleable, wk_linestring_filter(writer, as.integer(feature_id)), ...)
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_linestring
#' @export
wk_polygon <- function(handleable, feature_id = 1L, ring_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(
    handleable,
    wk_polygon_filter(
      writer,
      as.integer(feature_id),
      as.integer(ring_id)
    ),
    ...
  )
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_linestring
#' @export
wk_collection <- function(handleable, geometry_type = wk_geometry_type("geometrycollection"),
                               feature_id = 1L, ...) {
  writer <- wk_writer(handleable, generic = TRUE)
  result <- wk_handle(
    handleable,
    wk_collection_filter(
      writer,
      as.integer(geometry_type)[1],
      as.integer(feature_id)
    ),
    ...
  )
  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_linestring
#' @export
wk_linestring_filter <- function(handler, feature_id = 1L) {
  new_wk_handler(
    .Call(wk_c_linestring_filter_new, as_wk_handler(handler), feature_id),
    "wk_linestring_filter"
  )
}

#' @rdname wk_linestring
#' @export
wk_polygon_filter <- function(handler, feature_id = 1L, ring_id = 1L) {
  new_wk_handler(
    .Call(wk_c_polygon_filter_new, handler, feature_id, ring_id),
    "wk_polygon_filter"
  )
}

#' @rdname wk_linestring
#' @export
wk_collection_filter <- function(handler, geometry_type = wk_geometry_type("geometrycollection"),
                                 feature_id = 1L) {
  new_wk_handler(
    .Call(wk_c_collection_filter_new, as_wk_handler(handler), geometry_type, feature_id),
    "wk_collection_filter"
  )
}
