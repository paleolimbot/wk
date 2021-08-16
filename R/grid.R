
#' Raster-like objects
#'
#' @param data An object with two or more dimensions. Most usefully, a matrix.
#' @param bbox A [rct()] containing the bounds and CRS of the object.
#' @param nx,ny,dx,dy Either a number of cells in the x- and y- directions
#'   or delta in the x- and y-directions (in which case `bbox` must
#'   be specified).
#' @param type Use "polygons" to return a grid whose objects can be
#'   represented using an [rct()]; use "centers" to return a grid whose
#'   objects are the center of the [rct()] grid; use "corners" to return
#'   a grid along the corners of `bbox`.
#' @param x An object to convert to a grid
#' @param ... Passed to S3 methods
#'
#' @return
#'   - `wk_grid()` returns a `wk_grid_rct()` for `type == "polygons` or
#'     a `wk_grid_xy()` otherwise.
#'   - `wk_grid_rct()` returns an object of class "wk_grid_rct".
#'   - `wk_grid_xy()` returns an object of class "wk_grid_xy".
#' @export
#'
#' @examples
#' wk_grid_rct(volcano)
#' # approx bounding box in New Zealand Transverse Mercator
#' bbox <- wk::rct(5917019, 1756993, 5917466, 1757577, crs = "EPSG:2193")
#' wk_grid_rct(volcano, bbox)
#'
wk_grid <- function(bbox = NULL, nx = NULL, ny = NULL, dx = NULL, dy = NULL,
                    type = c("polygons", "corners", "centers")) {
  if (is.null(bbox)) {
    bbox <- NULL
  } else if (inherits(bbox, "wk_rct")) {
    bbox
  } else {
    wk_bbox(bbox)
  }
  type <- match.arg(type)

  if (is.null(nx) && is.null(ny) && !is.null(bbox)) {
    rct <- unclass(bbox)
    width <- rct$xmax - rct$xmin
    height <- rct$ymax - rct$ymin

    if (type == "polygons") {
      nx <- width / dx
      ny <- height / dy
    } else if (type == "corners") {
      nx <- width / dx + 1
      ny <- height / dy + 1
    } else if (type == "centers") {
      nx <- width / dx
      ny <- height / dy
      bbox <- rct(
        rct$xmin + dx / 2,
        rct$ymin + dy / 2,
        rct$xmax - dx / 2,
        rct$ymax - dy / 2,
        crs = wk_crs(bbox)
      )
    }
  } else if (is.null(dx) && is.null(dy)) {
    if (is.null(bbox)) {
      bbox <- rct(0, 0, nx, ny)
    }

    nx <- nx
    ny <- ny

    if (type == "centers") {
      rct <- unclass(bbox)
      width <- rct$xmax - rct$xmin
      height <- rct$ymax - rct$ymin
      dx <- width / nx
      dy <- height / ny

      bbox <- rct(
        rct$xmin + dx / 2,
        rct$ymin + dy / 2,
        rct$xmax - dx / 2,
        rct$ymax - dy / 2,
        crs = wk_crs(bbox)
      )
    }
  } else {
    stop(
      "Must specify dx, dy, and bbox OR nx and ny.",
      call. = FALSE
    )
  }

  # use a length-zero logical() with correct x and y dims
  data <- array(dim = c(nx, ny, 0))

  if (type == "polygons") {
    wk_grid_rct(data, bbox)
  } else {
    wk_grid_xy(data, bbox)
  }
}

#' @rdname wk_grid
#' @export
wk_grid_rct <- function(data, bbox = rct(0, 0, dim(data)[1], dim(data)[2])) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  # with zero values in the xy direction, bbox is empty
  if ((dim(data)[1] * dim(data)[2]) == 0) {
    return(
      new_wk_grid(
        list(
          data = data,
          bbox = wk::wk_bbox(xy(crs = wk_crs(bbox)))
        ),
        "wk_grid_rct"
      )
    )
  }

  new_wk_grid(list(data = data, bbox = bbox), "wk_grid_rct")
}

#' @rdname wk_grid
#' @export
wk_grid_xy <- function(data, bbox = rct(0, 0, dim(data)[1], dim(data)[2])) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  # with zero values in the xy direction, bbox is empty
  if ((dim(data)[1] * dim(data)[2]) == 0) {
    return(
      new_wk_grid(
        list(
          data = data,
          bbox = wk::wk_bbox(xy(crs = wk_crs(bbox)))
        ),
        "wk_grid_xy"
      )
    )
  }

  # with one value in the x dimension, we need a zero width bbox
  if (dim(data)[1] == 1) {
    stopifnot(
      unclass(bbox)$xmax == unclass(bbox)$xmin
    )
  }

  # with one value in the y dimension, we need a zero height bbox
  if (dim(data)[2] == 1) {
    stopifnot(
      unclass(bbox)$ymax == unclass(bbox)$ymin
    )
  }

  new_wk_grid(list(data = data, bbox = bbox), "wk_grid_xy")
}

#' @rdname wk_grid
#' @export
as_wk_grid_rct <- function(x, ...) {
  UseMethod("as_wk_grid_rct")
}

#' @rdname wk_grid
#' @export
as_wk_grid_rct.wk_grid_rct <- function(x, ...) {
  x
}

#' @rdname wk_grid
#' @export
as_wk_grid_xy <- function(x, ...) {
  UseMethod("as_wk_grid_xy")
}

#' @rdname wk_grid
#' @export
as_wk_grid_xy.wk_grid_xy <- function(x, ...) {
  x
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
