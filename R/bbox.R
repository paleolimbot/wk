
#' 2D bounding rectangles
#'
#' @inheritParams wk_handle
#'
#' @return A [rct()] of length 1.
#' @export
#'
#' @examples
#' wk_bbox(wkt("LINESTRING (1 2, 3 5)"))
#'
wk_bbox <- function(handleable) {
  UseMethod("wk_bbox")
}

#' @rdname wk_bbox
#' @export
wk_bbox.default <- function(handleable) {
  result <- wk_handle(handleable, wk_bbox_handler())
  wk_crs(result) <- wk_crs(handleable)
  result
}

# Note to future self: re-implementing wk_bbox() using range()
# for record-style vectors is not faster than the default method

#' @rdname wk_bbox
#' @export
wk_bbox_handler <- function() {
  new_wk_handler(.Call(wk_c_bbox_handler_new))
}
