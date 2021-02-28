
#' Extract feature-level meta
#'
#' @inheritParams wk_handle
#'
#' @return A data.frame with columns:
#'   - `geometry_type`: An integer identifying the geometry type
#'   - `size`: For points and linestrings, the number of coordinates; for
#'     polygons, the number of rings; for collections, the number of
#'     child geometries. A value of zero indicates an EMPTY geometry.
#'     A value of `NA` means this value is unknown without parsing the
#'     entire geometry.
#'   - `has_z`: `TRUE` if coordinates contain a Z value
#'   - `has_m`: `TRUE` if coordinates contain an M value
#'   - `srid`: An integer identifying a CRS or NA if this value was not
#'     provided.
#'   - `precision`: A grid size or 0.0 if a grid size was not provided.
#' @export
#'
#' @examples
#' wk_meta(as_wkt("LINESTRING (0 0, 1 1)"))
#' wk_meta(as_wkb("LINESTRING (0 0, 1 1)"))
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
wk_meta_handler <- function() {
  new_wk_handler(.Call(wk_c_meta_handler_new), "wk_meta_handler")
}
