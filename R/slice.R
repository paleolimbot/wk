
#' Handle specific regions of objects
#'
#' @inheritParams wk_handle
#' @param from 1-based index of the feature to start from
#' @param to 1-based index of the feature to end at
#' @param times The number of times the entire handleable should be
#'   repeated.
#' @param each The number of times each feature should be repeated.
#'
#' @return A subset of `handleable`
#' @export
#'
#' @examples
#' wk_slice(xy(1:5, 1:5), from = 3, to = 5)
#' wk_slice(
#'   data.frame(let = letters[1:5], geom = xy(1:5, 1:5)),
#'   from = 3, to = 5
#' )
#'
wk_slice <- function(handleable,
                     from = NULL, to = NULL, times = 1L, each = 1L,
                     handler = wk_writer(handleable), ...) {
  UseMethod("wk_slice")
}

#' @rdname wk_slice
#' @export
wk_slice.default <- function(handleable,
                             from = NULL, to = NULL, times = 1L, each = 1L,
                             handler = wk_writer(handleable), ...) {
  # make sure we're dealing with a handleable and a vector
  stopifnot(is_handleable(handleable), is_vector_class(handleable))

  from <- from %||% 1L
  to <- to %||% length(handleable)
  from <- max(from, 1L)
  to <- min(to, length(handleable))

  if (to >= from) {
    wk_handle(handleable[rep(rep(from:to, each = each), times = times)], handler, ...)
  } else {
    wk_handle(handleable[integer(0)], handler, ...)
  }
}
