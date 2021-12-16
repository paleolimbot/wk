
#' @rdname wk_writer
#' @export
wkt_writer <- function(precision = 16L, trim = TRUE) {
  new_wk_handler(
    .Call(
      wk_c_wkt_writer,
      as.integer(precision)[1],
      as.logical(trim)[1]
    ),
    "wk_wkt_writer"
  )
}
