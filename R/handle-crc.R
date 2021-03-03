
#' @rdname wk_handle
#' @export
wk_handle.wk_crc <- function(handleable, handler, ...,
                             n_segments = getOption("wk.crc_n_segments", NULL),
                             resolution = getOption("wk.crc_resolution", NULL)) {
  if (is.null(n_segments) && is.null(resolution)) {
    n_segments <- 100L
  } else if (is.null(n_segments)) {
    n_segments <- ceiling(2 * pi / (resolution / unclass(handleable)$r))
  }

  n_segments <- as.integer(pmax(4L, n_segments))
  n_segments[is.na(n_segments)] <- 4L

  if ((length(n_segments) != 1) && (length(n_segments) != length(handleable))) {
    stop(
      sprintf(
        "`n_segments`/`resolution` must be length 1 or length of data (%s)",
        length(handleable)
      ),
      call. = FALSE
    )
  }

  handler <- as_wk_handler(handler)
  .Call(wk_c_read_crc, handleable, handler, n_segments)
}
