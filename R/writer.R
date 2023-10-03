
#' Write geometry vectors
#'
#' When writing transformation functions, it is often useful to know which
#' handler should be used to create a (potentially modified) version
#' of an object. Some transformers (e.g., [wk_vertices()]) modify
#' the geometry type of an object, in which case a generic writer is needed.
#' This defaults to [wkb_writer()] because it is fast and can handle
#' all geometry types.
#'
#' @inheritParams wk_handle
#' @param precision If `trim` is `TRUE`, the total number of significant digits to keep
#'   for each result or the number of digits after the decimal place otherwise.
#' @param trim Use `FALSE` to keep trailing zeroes after the decimal place.
#' @param endian Use 1 for little endian, 0 for big endian, or NA for
#'   system endian.
#' @param generic Use `TRUE` to obtain a writer that can write all geometry
#'   types.
#' @param buffer_size Control the initial buffer size used when writing WKB.
#' @param promote_multi Use TRUE to promote all simple geometries to a multi
#'   type when reading to sfc. This is useful to increase the likelihood that
#'   the sfc will contain a single geometry type.
#' @param ... Passed to the writer constructor.
#'
#' @return A [wk_handler][wk_handle].
#' @export
#'
wk_writer <- function(handleable, ..., generic = FALSE) {
  UseMethod("wk_writer")
}

#' @rdname wk_writer
#' @export
wk_writer.default <- function(handleable, ...) {
  wkb_writer()
}

#' @rdname wk_writer
#' @export
wk_writer.wk_wkt <- function(handleable, ..., precision = 16, trim = TRUE) {
  wkt_writer(precision, trim)
}

#' @rdname wk_writer
#' @export
wk_writer.wk_wkb <- function(handleable, ...) {
  wkb_writer()
}

#' @rdname wk_writer
#' @export
wk_writer.wk_xy <- function(handleable, ..., generic = FALSE) {
  if (generic) wkb_writer() else xy_writer()
}
