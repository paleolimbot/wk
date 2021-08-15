
#' Raster-like objects
#'
#' @param data An object with two or more dimensions. Most usefully, a matrix.
#' @param bbox A [rct()] with bounds.
#'
#' @return
#' @export
#'
#' @examples
#' wk_grid(volcano)
#' # approx bounding box in New Zealand Transverse Mercator
#' bbox <- wk::rct(5917019, 1756993, 5917466, 1757577, crs = "EPSG:2193")
#' wk_grid(volcano, bbox)
#'
wk_grid_rct <- function(data, bbox = rct(0, 0, dim(data)[1], dim(data)[2])) {
  bbox <- as_rct(bbox)
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  new_wk_grid(list(data = data, bbox = bbox), "wk_grid_rct")
}

#' @rdname wk_grid_rct
#' @export
wk_grid_xy <- function(data, bbox = rct(0, 0, dim(data)[1], dim(data)[2])) {
  bbox <- as_rct(bbox)
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  new_wk_grid(list(data = data, bbox = bbox), "wk_grid_xy")
}

#' S3 details for grid objects
#'
#' @param x A [wk_grid()]
#' @param subclass An optional subclass.
#'
#' @export
#'
new_wk_grid <- function(x, subclass = character()) {
  structure(x, class = union(subclass, "wk_grid"))
}

#' @export
wk_bbox.wk_grid <- function(handleable, ...) {
  # take the bbox of the bbox to normalize a bounding box
  # with xmin > xmax
  wk_bbox(handleable$bbox)
}

#' @export
wk_crs.wk_grid <- function(x) {
  attr(x$bbox, "crs", exact = TRUE)
}

#' @export
wk_set_crs.wk_grid <- function(x, crs) {
  x$bbox <- wk_set_crs(x$bbox, crs)
  x
}

#' @export
format.wk_grid <- function(x, ...) {
  crs <- wk_crs(x)
  sprintf(
    "<%s [%s] => %s%s>",
    class(x)[1],
    paste0(dim(x$data), collapse = " x "),
    wk_bbox(x),
    if (is.null(crs)) "" else paste0(" with crs=", format(crs))
  )
}

#' @export
print.wk_grid <- function(x, ...) {
  cat(paste0(format(x), "\n"))
  utils::str(x)
  invisible(x)
}
