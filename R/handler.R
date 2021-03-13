
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
  cat(sprintf("<%s at %s>\n", class(x)[1], .Call(wk_c_handler_addr, x)))
  invisible(x)
}
