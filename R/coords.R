
#' Extract coordinates from well-known geometries
#'
#' @inheritParams wkb_translate_wkt
#'
#' @return A data.frame with columns:
#' - `feature_id`: The index of the top-level feature
#' - `nest_id`: The recursion level (if feature is a geometry collection)
#' - `part_id`: The part index (if nested within a multi-geometry or collection)
#' - `ring_id`: The ring index
#' - `coordinate_id`: The coordinate index
#' - `x`, `y`, `z`, `m`: Coordinaate values (both absence and `nan` are recorded
#'   as `NA`)
#'
#' @export
#'
#' @examples
#' wkt_coords("LINESTRING (0 1, 19 27)")
#'
wkb_coords <- function(wkb) {
  coords <- cpp_coords_wkb(wkb)
  # slightly faster than data.frame()
  structure(coords, row.names = seq_len(length(coords[[1]])), class = "data.frame")
}

#' @rdname wkb_coords
#' @export
wkt_coords <- function(wkt) {
  coords <- cpp_coords_wkt(wkt)
  # slightly faster than data.frame()
  structure(coords, row.names = seq_len(length(coords[[1]])), class = "data.frame")
}
