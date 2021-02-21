
#' @rdname wk_writer
#' @export
xyzm_writer <- function() {
  new_wk_handler(.Call(wk_c_xyzm_writer_new), "wk_xyzm_writer")
}
