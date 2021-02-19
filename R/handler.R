
#' Read geometry vectors
#'
#' The handler is the basic building block of the wk package. In
#' particular, the [wk_handle()] generic allows operations written
#' as handlers to "just work" with many different input types. The
#' wk package provides the [wk_void()] handler, the [wk_format()]
#' handler, the [wk_debug()] handler, the [wk_problems()] handler,
#' and [wk_writer()]s for [wkb()], [wkt()], [xy()], and [sf::st_sfc()])
#' vectors.
#'
#' @param handler_ptr An external pointer to a newly created WK handler
#' @param handler A [wk_handler][wk_handle] object.
#' @param subclass The handler subclass
#' @param handleable A geometry vector (e.g., [wkb()], [wkt()], [xy()],
#'   [rct()], or [sf::st_sfc()]) for which [wk_handle()] is defined.
#' @param n_segments,resolution The number of segments to use when approximating
#'   a circle. The default uses `getOption("wk.crc_n_segments")` so that
#'   this value can be set for implicit conversions (e.g., `as_wkb()`).
#'   Alternatively, set the minimum distance between points on the circle
#'   (used to estimate `n_segments`). The default is obtained
#'   using `getOption("wk.crc_resolution")`.
#' @param ... Passed to the [wk_handle()] method.
#'
#' @return A WK handler.
#' @export
#'
wk_handle <- function(handleable, handler, ...) {
  UseMethod("wk_handle")
}

#' @rdname wk_handle
#' @export
wk_handle.wk_wkb <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_wkb, handleable, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(handleable, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_xy <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_xy, handleable, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_rct <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_rct, handleable, handler)
}

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

  if (length(n_segments) != length(handleable)) {
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

#' @rdname wk_handle
#' @export
new_wk_handler <- function(handler_ptr, subclass = character()) {
  stopifnot(typeof(handler_ptr) == "externalptr")
  structure(handler_ptr, class = union(subclass, "wk_handler"))
}

#' @rdname wk_handle
#' @export
is_wk_handler <- function(handler) {
  inherits(handler, "wk_handler")
}

#' @rdname wk_handle
#' @export
as_wk_handler <- function(handler, ...) {
  if (is.function(handler)) handler() else handler
}

#' @export
print.wk_handler <- function(x, ...) {
  cat(sprintf("<%s at %s>\n", class(x)[1], .Call(wk_c_handler_addr)))
  invisible(x)
}
