
#' Test handlers
#'
#' @param handler_ptr An external pointer to a newly created WK handler
#' @param handler An object created with [new_wk_handler()].
#' @param subclass The handler subclass
#' @param precision If `trim` is `TRUE`, the total number of significant digits to keep
#'   for each result or the number of digits after the decimal place otherwise.
#' @param trim Use `FALSE` to keep trailing zeroes after the decimal place.
#' @param x A geometry vector (e.g., [wkb()] or [wkt()]).
#' @param ... Unused
#' @return A WK handler.
#' @export
#'
wk_void_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_void_new), "wk_void_handler")
}

#' @rdname wk_void_handler
#' @export
wk_void <- function(x, ...) {
  invisible(wk_handle(x, wk_void_handler(), ...))
}



#' @rdname wk_void_handler
#' @export
wk_problems_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_problems_new), "wk_problems_handler")
}

#' @rdname wk_void_handler
#' @export
wk_problems <- function(x, ...) {
  wk_handle(x, wk_problems_handler(), ...)
}

#' @rdname wk_void_handler
#' @export
wkt_writer <- function(precision = 16, trim = TRUE) {
  new_wk_handler(wk_cpp_wkt_writer(precision, trim), "wk_wkt_writer")
}

#' @rdname wk_void_handler
#' @export
wkb_writer <- function() {
  new_wk_handler(.Call(wk_c_wkb_writer_new), "wk_wkb_writer")
}

#' @rdname wk_void_handler
#' @export
sfc_writer <- function() {
  new_wk_handler(.Call(wk_c_sfc_writer_new), "wk_sfc_writer")
}

#' @rdname wk_void_handler
#' @export
xyzm_writer <- function() {
  new_wk_handler(.Call(wk_c_xyzm_writer_new), "wk_xyzm_writer")
}

#' @rdname wk_void_handler
#' @export
new_wk_handler <- function(handler_ptr, subclass = character()) {
  stopifnot(typeof(handler_ptr) == "externalptr")
  structure(handler_ptr, class = union(subclass, "wk_handler"))
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

#' @export
as_wk_handler.function <- function(handler, ...) {
  handler()
}

#' @rdname wk_void_handler
#' @export
wk_handle <- function(x, handler, ...) {
  UseMethod("wk_handle")
}

#' @rdname wk_void_handler
#' @export
wk_handle.wk_wkb <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_wkb, x, handler)
}

#' @rdname wk_void_handler
#' @export
wk_handle.wk_wkt <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  wk_cpp_handle_wkt(x, handler)
}

#' @rdname wk_void_handler
#' @export
wk_handle.wk_xy <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_xy, x, handler)
}

#' @rdname wk_void_handler
#' @export
wk_handle.wk_rct <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_rct, x, handler)
}

#' @rdname wk_void_handler
#' @export
wk_handle.sfc <- function(x, handler, ...) {
  handler <- as_wk_handler(handler)
  .Call(wk_c_read_sfc, x, handler)
}

#' @export
print.wk_handler <- function(x, ...) {
  cat(sprintf("<%s at %s>\n", class(x)[1], .Call(wk_c_handler_addr)))
  invisible(x)
}
