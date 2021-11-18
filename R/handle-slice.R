
#' Handle specific regions of objects
#'
#' @inheritParams wk_handle
#' @param from 1-based index of the feature to start from
#' @param to 1-based index of the feature to end at
#'
#' @return A subset of `handleable`
#' @export
#'
#' @examples
#' wk_handle_slice(xy(1:5, 1:5), from = 3, to = 5)
#' wk_handle_slice(
#'   data.frame(let = letters[1:5], geom = xy(1:5, 1:5)),
#'   from = 3, to = 5
#' )
#'
wk_handle_slice <- function(handleable, handler = wk_writer(handleable),
                            from = NULL, to = NULL, ...) {
  UseMethod("wk_handle_slice")
}

#' @rdname wk_handle_slice
#' @export
wk_handle_slice.default <- function(handleable, handler = wk_writer(handleable),
                                    from = NULL, to = NULL, ...) {
  # make sure we're dealing with a handleable and a vector
  stopifnot(is_handleable(handleable), is_vector_class(handleable))

  from <- from %||% 1L
  to <- to %||% length(handleable)
  from <- max(from, 1L)
  to <- min(to, length(handleable))

  if (to >= from) {
    wk_handle(handleable[from:to], handler, ...)
  } else {
    wk_handle(handleable[integer(0)], handler, ...)
  }
}
