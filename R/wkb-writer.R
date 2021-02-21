
#' @rdname wk_writer
#' @export
wkb_writer <- function(buffer_size = 2048L, endian = NA_integer_) {
  new_wk_handler(
    .Call(wk_c_wkb_writer_new, as.integer(buffer_size), as.integer(endian)),
    "wk_wkb_writer"
  )
}
