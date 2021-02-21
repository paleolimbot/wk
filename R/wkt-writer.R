
#' @rdname wk_writer
#' @export
wkt_writer <- function(precision = 16L, trim = TRUE) {
  new_wk_handler(wk_cpp_wkt_writer(precision, trim), "wk_wkt_writer")
}
