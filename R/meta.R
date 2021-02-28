
#' Extract feature-level meta
#'
#' These functions return the non-coordinate information of a geometry
#' and/or vector. They do not parse an entire geometry/vector and are
#' intended to be very fast even for large vectors.
#'
#' @inheritParams wk_handle
#' @param geometry_type An integer code for the geometry type. These
#'   integers follow the WKB specification (e.g., 1 for point,
#'   7 for geometrycollection).
#' @param geometry_type_label A character vector of (lowercase)
#'   geometry type labels as would be found in WKT (e.g., point,
#'   geometrycollection).
#'
#' @return A data.frame with columns:
#'   - `geometry_type`: An integer identifying the geometry type.
#'     A value of 0 indicates that the types of geometry in the vector
#'     are not known without parsing the entire vector.
#'   - `size`: For points and linestrings, the number of coordinates; for
#'     polygons, the number of rings; for collections, the number of
#'     child geometries. A value of zero indicates an EMPTY geometry.
#'     A value of `NA` means this value is unknown without parsing the
#'     entire geometry.
#'   - `has_z`: `TRUE` if coordinates contain a Z value. A value of `NA`
#'     means this value is unknown without parsing the entire vector.
#'   - `has_m`: `TRUE` if coordinates contain an M value. A value of `NA`
#'     means this value is unknown without parsing the entire vector.
#'   - `srid`: An integer identifying a CRS or NA if this value was not
#'     provided.
#'   - `precision`: A grid size or 0.0 if a grid size was not provided.
#'     Note that coordinate values may not have been rounded; the grid
#'     size only refers to the level of detail with which they should
#'     be interpreted.
#'
#' @export
#'
#' @examples
#' wk_vector_meta(as_wkt("LINESTRING (0 0, 1 1)"))
#' wk_meta(as_wkt("LINESTRING (0 0, 1 1)"))
#' wk_meta(as_wkb("LINESTRING (0 0, 1 1)"))
#'
#' wk_geometry_type_label(1:7)
#' wk_geometry_type(c("point", "geometrycollection"))
#'
wk_meta <- function(handleable, ...) {
  UseMethod("wk_meta")
}

#' @rdname wk_meta
#' @export
wk_meta.default <- function(handleable, ...) {
  new_data_frame(wk_handle(handleable, wk_meta_handler(), ...))
}

#' @rdname wk_meta
#' @export
wk_vector_meta <- function(handleable, ...) {
  UseMethod("wk_vector_meta")
}

#' @rdname wk_meta
#' @export
wk_vector_meta.default <- function(handleable, ...) {
  new_data_frame(wk_handle(handleable, wk_vector_meta_handler(), ...))
}

#' @rdname wk_meta
#' @export
wk_meta_handler <- function() {
  new_wk_handler(.Call(wk_c_meta_handler_new), "wk_meta_handler")
}

#' @rdname wk_meta
#' @export
wk_vector_meta_handler <- function() {
  new_wk_handler(.Call(wk_c_vector_meta_handler_new), "wk_vector_meta_handler")
}

#' @rdname wk_meta
#' @export
wk_geometry_type_label <- function(geometry_type) {
  c(
    "point", "linestring", "polygon",
    "multipoint", "multilinestring", "multipolygon",
    "geometrycollection"
  )[as.integer(geometry_type)]
}

#' @rdname wk_meta
#' @export
wk_geometry_type <- function(geometry_type_label) {
  match(
    geometry_type_label,
    c(
      "point", "linestring", "polygon",
      "multipoint", "multilinestring", "multipolygon",
      "geometrycollection"
    )
  )
}
