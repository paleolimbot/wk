
#' Write geometry vectors
#'
#' @param handleable Placeholder
#' @param precision If `trim` is `TRUE`, the total number of significant digits to keep
#'   for each result or the number of digits after the decimal place otherwise.
#' @param trim Use `FALSE` to keep trailing zeroes after the decimal place.
#' @param ... For S3 generic compatibility
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
wk_writer.sfc <- function(handleable, ...) {
  sfc_writer()
}

#' @rdname wk_writer
#' @export
wk_writer.wk_xy <- function(handleable, ...) {
  xyzm_writer()
}

#' @rdname wk_writer
#' @export
wkt_writer <- function(precision = 16, trim = TRUE) {
  new_wk_handler(wk_cpp_wkt_writer(precision, trim), "wk_wkt_writer")
}

#' @rdname wk_writer
#' @export
wkb_writer <- function() {
  new_wk_handler(.Call(wk_c_wkb_writer_new), "wk_wkb_writer")
}

#' @rdname wk_writer
#' @export
sfc_writer <- function() {
  new_wk_handler(.Call(wk_c_sfc_writer_new), "wk_sfc_writer")
}

#' @rdname wk_writer
#' @export
xyzm_writer <- function() {
  new_wk_handler(.Call(wk_c_xyzm_writer_new), "wk_xyzm_writer")
}
