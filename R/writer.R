
#' Write geometry vectors
#'
#' @inheritParams wk_handle
#' @param precision If `trim` is `TRUE`, the total number of significant digits to keep
#'   for each result or the number of digits after the decimal place otherwise.
#' @param trim Use `FALSE` to keep trailing zeroes after the decimal place.
#' @param endian Use 1 for little endian, 0 for big endian, or NA for
#'   system endian.
#' @param buffer_size Control the initial buffer size used when writing WKB.
#' @param ... Passed to the writer constructor.
#'
#' @return A [wk_handler][wk_handle].
#' @export
#'
wk_writer <- function(handleable, ...) {
  UseMethod("wk_writer")
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
wk_writer.wk_xy <- function(handleable, ...) {
  xy_writer()
}
