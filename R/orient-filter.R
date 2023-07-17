
#' Orient polygon coordinates
#'
#' @inheritParams wk_handle
#' @param direction The winding polygon winding direction
#'
#' @return `handleable` with consistently oriented polygons, in `direction`
#'   winding order.
#' @export
#'
#' @examples
#' wk_orient(wkt("POLYGON ((0 0, 1 0, 1 1, 0 1, 0 0))"))
#' wk_orient(
#'   wkt("POLYGON ((0 0, 0 1, 1 1, 1 0, 0 0))"),
#'   direction = wk_clockwise()
#' )
#'
wk_orient <- function(handleable, ..., direction = wk_counterclockwise()) {
  result <- wk_handle(
    handleable,
    wk_orient_filter(wk_writer(handleable), direction),
    ...
  )
  wk_restore(handleable, result, ...)
}

#' @rdname wk_orient
#' @export
wk_orient_filter <- function(handler, direction = wk_counterclockwise()) {
  stopifnot(direction %in% c(wk_clockwise(), wk_counterclockwise()))

  new_wk_handler(
    .Call(
      wk_c_orient_filter_new,
      as_wk_handler(handler),
      as.integer(direction)[1]
    ),
    "wk_orient_filter"
  )
}

#' @rdname wk_orient
#' @export
wk_clockwise <- function() {
  -1L
}

#' @rdname wk_orient
#' @export
wk_counterclockwise <- function() {
  1L
}
