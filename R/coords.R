
#' Extract coordinates from well-known geometries
#'
#' These functions are optimised for graphics output,
#' which in R require flat coordinate structures. See
#' [graphics::points()], [graphics::lines()],
#' and  [graphics::polypath()] for how to send these
#' to a graphics device, or [grid::pointsGrob()],
#' [grid::linesGrob()], and [grid::pathGrob()] for how
#' to create graphical objects using this output.
#'
#' @inheritParams wkb_translate_wkt
#' @param sep_na Use `TRUE` to separate geometries and linear
#'   rings with a row of `NA`s. This is useful for generating
#'   output that can be fed directly to [graphics::polypath()]
#'   or [graphics::lines()] without modification.
#'
#' @return A data.frame with columns:
#' - `feature_id`: The index of the top-level feature
#' - `part_id`: The part identifier, guaranteed to be unique for every simple geometry
#'   (including those contained within a multi-geometry or collection)
#' - `ring_id`: The ring identifier, guaranteed to be unique for every ring.
#' - `x`, `y`, `z`, `m`: Coordinaate values (both absence and `nan` are recorded
#'   as `NA`)
#'
#' @export
#'
#' @examples
#' text <- c("LINESTRING (0 1, 19 27)", "LINESTRING (-1 -1, 4 10)")
#' wkt_coords(text)
#' wkt_coords(text, sep_na = TRUE)
#'
wkb_coords <- function(wkb, sep_na = FALSE) {
  new_data_frame(cpp_coords_wkb(wkb, sep_na))
}

#' @rdname wkb_coords
#' @export
wkt_coords <- function(wkt, sep_na = FALSE) {
  new_data_frame(cpp_coords_wkt(wkt, sep_na))
}

#' @rdname wkb_coords
#' @export
wksxp_coords <- function(wksxp, sep_na = FALSE) {
  new_data_frame(cpp_coords_wksxp(wksxp, sep_na))
}
