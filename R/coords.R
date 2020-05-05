
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
  new_data_frame(cpp_coords_wkb(wkb))
}

#' @rdname wkb_coords
#' @export
wkt_coords <- function(wkt) {
  new_data_frame(cpp_coords_wkt(wkt))
}

#' @rdname wkb_coords
#' @export
wksxp_coords <- function(wksxp) {
  new_data_frame(cpp_coords_wksxp(wksxp))
}
