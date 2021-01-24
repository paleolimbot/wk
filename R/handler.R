
#' Read geometry vectors
#'
#' @param handler_ptr An external pointer to a newly created WK handler
#' @param handler An object created with [new_wk_handler()].
#' @param subclass The handler subclass
#' @param x A geometry vector (e.g., [wkb()] or [wkt()]).
#' @param ... Unused
#'
#' @return A WK handler.
#' @export
#'
wk_handle <- function(x, handler, ...) {
  UseMethod("wk_handle")
}

#' @rdname wk_handle
#' @export
wk_handle.wk_wkb <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_wkb, x, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_wkt <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(x, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_xy <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_xy, x, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.wk_rct <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_rct, x, handler)
}

#' @rdname wk_handle
#' @export
wk_handle.sfc <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_sfc, x, handler)
}

#' @rdname wk_handle
#' @export
wk_void_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_void_new), "wk_void_handler")
}

#' @rdname wk_handle
#' @export
wk_void <- function(x, ...) {
  invisible(wk_handle(x, wk_void_handler(), ...))
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
