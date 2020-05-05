
#' Extract meta information
#'
#' @inheritParams wkb_translate_wkt
#' @param recursive Pass `TRUE` to recurse into multi-geometries
#'   and collections to extract meta of sub-geometries
#' @param type A string version of the geometry type (e.g.,
#'   point, linestring, polygon, multipoint, multilinestring,
#'   multipolygon, geometrycollection)
#' @param type_id An integer version of the geometry type
#'
#' @return A data.frame with columns:
#' - `feature_id`: The index of the top-level feature
#' - `nest_id`: The recursion level (if feature is a geometry collection)
#' - `part_id`: The part index (if nested within a multi-geometry or collection)
#' - `type_id`: The type identifier (see [wk_geometry_type()])
#' - `size`: For points and linestrings the number of points, for polygons
#'   the number of rings, and for mutlti-geometries and collection types,
#'   the number of child geometries.
#' - `srid`: The spatial reference identifier as an integer
#'
#' @export
#'
#' @examples
#' wkt_meta("POINT (30 10)")
#' wkt_meta("GEOMETRYCOLLECTION (POINT (30 10))", recursive = FALSE)
#' wkt_meta("GEOMETRYCOLLECTION (POINT (30 10))", recursive = TRUE)
#'
wkb_meta <- function(wkb, recursive = FALSE) {
  meta <- cpp_meta_wkb(wkb, recursive = recursive)
  # slightly faster than data.frame()
  structure(meta, row.names = seq_len(length(meta[[1]])), class = "data.frame")
}

#' @rdname wkb_meta
#' @export
wkt_meta <- function(wkt, recursive = FALSE) {
  meta <- cpp_meta_wkt(wkt, recursive = recursive)
  # slightly faster than data.frame()
  structure(meta, row.names = seq_len(length(meta[[1]])), class = "data.frame")
}

#' @rdname wkb_meta
#' @export
wkt_streamer_meta <- function(wkt, recursive = FALSE) {
  meta <- cpp_meta_wkt_streamer(wkt, recursive = recursive)
  # slightly faster than data.frame()
  structure(meta, row.names = seq_len(length(meta[[1]])), class = "data.frame")
}

#' @rdname wkb_meta
#' @export
wksxp_meta <- function(wksxp, recursive = FALSE) {
  meta <- cpp_meta_wksxp(wksxp, recursive = recursive)
  # slightly faster than data.frame()
  structure(meta, row.names = seq_len(length(meta[[1]])), class = "data.frame")
}

#' @rdname wkb_meta
#' @export
wk_geometry_type <- function(type_id) {
  c(
    "point", "linestring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[as.integer(type_id)]
}

#' @rdname wkb_meta
#' @export
wk_geometry_type_id <- function(type) {
  match(
    type,
    c(
      "point", "linestring", "polygon",
      "multipoint", "multilinestring", "multipolygon",
      "geometrycollection"
    )
  )
}
