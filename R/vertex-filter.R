
#' Extract vertices
#'
#' These functions provide ways to extract individual coordinate values.
#' Whereas `wk_vertices()` returns a vector of coordinates as in the same
#' format as the input, `wk_coords()` returns a data frame with coordinates
#' as columns.
#'
#' `wk_coords<-` is the replacement-function version of 'wk_coords'.
#' Using the engine of [wk_trans_explicit()] the coordinates of an object
#' can be transformed in a generic way using R functions as needed.
#'
#' @inheritParams wk_handle
#' @param add_details Use `TRUE` to add a "wk_details" attribute, which
#'   contains columns `feature_id`, `part_id`, and `ring_id`.
#' @inheritParams wk_trans_set
#'
#' @return
#'   - `wk_vertices()` extracts vertices and returns the in the same format as
#'     the handler
#'   - `wk_coords()` returns a data frame with columns columns `feature_id`
#'     (the index of the feature from whence it came), `part_id` (an arbitrary
#'     integer identifying the point, line, or polygon from whence it came),
#'     `ring_id` (an arbitrary integer identifying individual rings within
#'     polygons), and one column per coordinate (`x`, `y`, and/or `z` and/or `m`).
#' @export
#'
#' @examples
#' wk_vertices(wkt("LINESTRING (0 0, 1 1)"))
#' wk_coords(wkt("LINESTRING (0 0, 1 1)"))
#'
#' # wk_coords() replacement function
#' x <- xy(1:5, 1:5)
#' y <- as_wkt(x)
#' wk_coords(y) <- cbind(5:1, 0:4)
#' wk_coords(x) <- y[5:1]
#' y
#' x
#'
wk_vertices <- function(handleable, ...) {
  # the results of this handler are not necessarily the same length as the input,
  # so we need to special-case data frames
  if (is.data.frame(handleable))  {
    result <- wk_handle(
      handleable,
      wk_vertex_filter(wk_writer(handleable), add_details = TRUE),
      ...
    )
    feature_id <- attr(result, "wk_details", exact = TRUE)$feature_id
    attr(result, "wk_details") <- NULL
    result <- wk_restore(handleable[feature_id, , drop = FALSE], result, ...)
  } else {
    result <- wk_handle(handleable, wk_vertex_filter(wk_writer(handleable, generic = TRUE)), ...)
    result <- wk_restore(handleable, result, ...)
  }

  wk_set_crs(result, wk_crs(handleable))
}

#' @rdname wk_vertices
#' @export
wk_coords <- function(handleable, ...) {
  UseMethod("wk_coords")
}

#' @export
wk_coords.default <- function(handleable, ...) {
  result <- wk_handle(
    handleable,
    wk_vertex_filter(xy_writer(), add_details = TRUE),
    ...
  )

  details <- attr(result, "wk_details", exact = TRUE)
  attr(result, "wk_details") <- NULL
  new_data_frame(c(details, unclass(result)))
}

#' @rdname wk_vertices
#' @export
`wk_coords<-` <- function(handleable, use_z = NA, use_m = NA, value) {
  wk_transform(handleable, wk_trans_explicit(value, use_z = use_z, use_m = use_m))
}

#' @rdname wk_vertices
#' @export
wk_vertex_filter <- function(handler, add_details = FALSE) {
  new_wk_handler(
    .Call("wk_c_vertex_filter_new", as_wk_handler(handler), as.logical(add_details)[1]),
    "wk_vertex_filter"
  )
}

#' @export
wk_coords.wk_xy <- function(handleable, ...) {
  feature_id <- seq_along(handleable)
  is_na <- Reduce("&", lapply(unclass(handleable), is.na))
  has_coord <- !is_na

  if (!all(has_coord)) {
    handleable <- handleable[has_coord]
    feature_id <- feature_id[has_coord]
  }

  new_data_frame(
    c(
      list(
        feature_id = feature_id,
        part_id = feature_id,
        ring_id = rep_len(0L, length(feature_id))
      ),
      unclass(handleable)
    )
  )
}
