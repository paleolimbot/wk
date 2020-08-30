
#' Test handlers
#'
#' @param x An external pointer to a newly created WK handler
#' @return A WK handler
#' @export
#'
wk_void_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_void_new), "wk_void_handler")
}

#' @rdname wk_void_handler
#' @export
wk_debug_handler <- function() {
  new_wk_handler(.Call(wk_c_handler_debug_new), "wk_debug_handler")
}

#' @rdname wk_void_handler
#' @export
new_wk_handler <- function(x, subclass = character(), finisher = wk_handler_default_finisher) {
  stopifnot(is.function(finisher))
  structure(x, class = union(subclass, "wk_handler"), finisher = finisher)
}

#' @rdname wk_void_handler
#' @export
wk_handler_default_finisher <- function(result, x) {
  if (inherits(result, "wk_error_sentinel")) {
    stop(result$message, call. = FALSE)
  } else {
    result
  }
}

#' @export
print.wk_handler <- function(x, ...) {
  cat(sprintf("<%s at %s> \n", class(x)[1], .Call(wk_c_handler_addr)))
  invisible(x)
}
