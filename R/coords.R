
#' Data frames of coordinates
#'
#' @inheritParams wkb_translate_wkt
#'
#' @return A data.frame with columns:
#' - `feature_id`
#' - `nest_id`
#' - `part_id`
#' - `ring_id`
#' - `coordinate_id`
#' - `x`
#' - `y`
#' - `z`
#' - `m`
#'
#' @export
#'
wkb_coords <- function(wkb) {
  coords <- cpp_coords_wkb(wkb)
  structure(coords, row.names = seq_len(length(coords[[1]])), class = "data.frame")
}

#' @rdname wkb_coords
#' @export
wkt_coords <- function(wkt) {
  cpp_coords_wkt(wkt)
}
