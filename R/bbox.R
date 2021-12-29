
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
wk_bbox <- function(handleable, ...) {
  UseMethod("wk_bbox")
}

#' @rdname wk_bbox
#' @export
wk_envelope <- function(handleable, ...) {
  UseMethod("wk_envelope")
}

#' @rdname wk_bbox
#' @export
wk_bbox.default <- function(handleable, ...) {
  if (wk_is_geodesic(handleable)) {
    stop("Can't compute bbox for geodesic object", call. = FALSE)
  }

  result <- wk_handle(handleable, wk_bbox_handler(), ...)
  wk_crs(result) <- wk_crs(handleable)
  result
}

#' @rdname wk_bbox
#' @export
wk_envelope.default <- function(handleable, ...) {
  if (wk_is_geodesic(handleable)) {
    stop("Can't compute envelope for geodesic object", call. = FALSE)
  }

  result <- wk_handle(handleable, wk_envelope_handler(), ...)
  wk_crs(result) <- wk_crs(handleable)
  result
}

#' @rdname wk_bbox
#' @export
wk_envelope.wk_rct <- function(handleable, ...) {
  handleable
}

#' @rdname wk_bbox
#' @export
wk_envelope.wk_crc <- function(handleable, ...) {
  unclassed <- unclass(handleable)

  rct_data <- list(
    xmin = unclassed$x - unclassed$r,
    ymin = unclassed$y - unclassed$r,
    xmax = unclassed$x + unclassed$r,
    ymax = unclassed$y + unclassed$r
  )

  new_wk_rct(rct_data, crs = attr(handleable, "crs", exact = TRUE))
}

#' @rdname wk_bbox
#' @export
wk_envelope.wk_xy <- function(handleable, ...) {
  unclassed <- unclass(handleable)
  rct_data <- c(unclassed[1:2], unclassed[1:2])
  names(rct_data) <- c("xmin", "ymin", "xmax", "ymax")
  new_wk_rct(rct_data, crs = attr(handleable, "crs", exact = TRUE))
}

# Note to future self: re-implementing wk_bbox() using range()
# for record-style vectors is not faster than the default method

#' @rdname wk_bbox
#' @export
wk_bbox_handler <- function() {
  new_wk_handler(.Call(wk_c_bbox_handler_new), "wk_bbox_handler")
}

#' @rdname wk_bbox
#' @export
wk_envelope_handler <- function() {
  new_wk_handler(.Call(wk_c_envelope_handler_new), "wk_envelope_handler")
}
