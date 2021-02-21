
#' @rdname wk_writer
#' @export
sfc_writer <- function() {
  new_wk_handler(.Call(wk_c_sfc_writer_new), "wk_sfc_writer")
}
