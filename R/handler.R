
#' Test handlers
#'
#' @param handler_ptr An external pointer to a newly created WK handler
#' @param handler An object created with [new_wk_handler()].
#' @param subclass The handler subclass
#' @param finisher,result A function called on the `result` of the handler.
#' @param x A vector that can be interpreted as a [wkb()], [wkt()], or [wksxp()].
#' @param ... Passed to the handler constructor.
#' @return A WK handler
#' @export
#'
wk_void_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_void_new), "wk_void_handler", finisher = wk_finish_invisible)
}

#' @rdname wk_void_handler
#' @export
wk_debug_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_debug_new), "wk_debug_handler", finisher = wk_finish_invisible)
}

#' @rdname wk_void_handler
#' @export
wk_validation_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_validation_new), "wk_validation_handler")
}

#' @rdname wk_void_handler
#' @export
new_wk_handler <- function(handler_ptr, subclass = character(), finisher = wk_finish_default) {
  stopifnot(is.function(finisher), typeof(handler_ptr) == "externalptr")
  structure(handler_ptr, class = union(subclass, "wk_handler"), finisher = finisher)
}

#' @rdname wk_void_handler
#' @export
is_wk_handler <- function(handler) {
  inherits(handler, "wk_handler")
}

#' @rdname wk_void_handler
#' @export
as_wk_handler <- function(handler, ...) {
  UseMethod("as_wk_handler")
}

#' @export
as_wk_handler.wk_handler <- function(handler, ...) {
  handler
}

#' @rdname wk_void_handler
#' @export
wk_finish_default <- function(result, x) {
  if (inherits(result, "wk_error_sentinel")) {
    stop(result$message, call. = FALSE)
  } else {
    result
  }
}

#' @rdname wk_void_handler
#' @export
wk_finish_invisible <- function(result, x) {
  invisible(wk_finish_default(result, x))
}

#' @rdname wk_void_handler
#' @export
handle_wkb <- function(x, handler, ...) {
  attr(handler, "finisher")(.Call(wk_c_read_wkb, as_wkb(x), as_wk_handler(handler, ...)))
}

#' @rdname wk_void_handler
#' @export
handle_wkt <- function(x, handler, ...) {
  attr(handler, "finisher")(wk_cpp_handle_wkt(as_wkt(x), as_wk_handler(handler, ...)))
}

#' @export
print.wk_handler <- function(x, ...) {
  cat(sprintf("<%s at %s>\n", class(x)[1], .Call(wk_c_handler_addr)))
  invisible(x)
}
