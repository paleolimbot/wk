
#' Extract simple geometries
#'
#' @inheritParams wk_handle
#' @param add_details Use `TRUE` to add a "wk_details" attribute, which
#'   contains columns `feature_id`, `part_id`, and `ring_id`.
#'
#' @return `handleable` transformed such that collections have been
#'   expanded and only simple geometries (point, linestring, polygon)
#'   remain.
#' @export
#'
#' @examples
#' wk_flatten(wkt("MULTIPOINT (1 1, 2 2, 3 3)"))
#'
wk_flatten <- function(handleable, ...) {
  if (is.data.frame(handleable))  {
    result <- wk_handle(
      handleable,
      wk_flatten_filter(wk_writer(handleable), add_details = TRUE),
      ...
    )
    feature_id <- attr(result, "wk_details", exact = TRUE)$feature_id
    attr(result, "wk_details") <- NULL
    result <- wk_restore(handleable[feature_id, , drop = FALSE], result, ...)
  } else {
    result <- wk_handle(handleable, wk_flatten_filter(wk_writer(handleable, generic = TRUE)), ...)
    result <- wk_restore(handleable, result, ...)
  }

  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_flatten
#' @export
wk_flatten_filter <- function(handler, add_details = FALSE) {
  new_wk_handler(
    .Call("wk_c_flatten_filter_new", as_wk_handler(handler), as.logical(add_details)[1]),
    "wk_flatten_filter"
  )
}
