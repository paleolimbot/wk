wk_orient <- function(handleable, ..., direction = wk_counterclockwise()) {
  result <- wk_handle(
    handleable,
    wk_orient_filter(wk_writer(handleable), direction),
    ...
  )
  wk_restore(handleable, result, ...)
}

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

wk_clockwise <- function() {
  -1L
}

wk_counterclockwise <- function() {
  1L
}
