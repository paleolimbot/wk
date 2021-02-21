
#' @rdname wk_writer
#' @export
xy_writer <- function() {
  new_wk_handler(.Call(wk_c_xy_writer_new), "wk_xy_writer")
}
