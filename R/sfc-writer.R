
#' @rdname wk_writer
#' @export
sfc_writer <- function(promote_multi = FALSE) {
  new_wk_handler(.Call(wk_c_sfc_writer_new, as.logical(promote_multi)[1]), "wk_sfc_writer")
}
