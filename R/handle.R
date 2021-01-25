
#' Read geometry vectors
#'
#' @param handler_ptr An external pointer to a newly created WK handler
#' @param handler A [wk_handler][wk_handle] object.
#' @param subclass The handler subclass
#' @param handleable A geometry vector (e.g., [wkb()], [wkt()], [xy()],
#'   [rct()], or [sf::st_sfc()]) for which [wk_handle()] is defined.
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
wk_handle.sfc <- function(handleable, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_sfc, handleable, handler)
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
