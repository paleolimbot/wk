
#' Raster-like objects
#'
#' [grd()] objects are just an array (any object with more than
#' two [dim()]s) and a bounding box (a [rct()], which may or
#' may not have a [wk_crs()] attached). The ordering of the dimensions
#' is y (indices increasing downwards), x (indices increasing to the right).
#' This follows the ordering of [as.raster()]/[rasterImage()] and aligns
#' with the printing of matrices.
#'
#' @param data An object with two or more dimensions. Most usefully, a matrix.
#' @param bbox A [rct()] containing the bounds and CRS of the object. You can
#'   specify a [rct()] with `xmin > xmax` or `ymin > ymax` which will flip
#'   the underlying data and return an object with a normalized bounding
#'   box and data.
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
#'   - `grd()` returns a `grd_rct()` for `type == "polygons` or
#'     a `grd_xy()` otherwise.
#'   - `grd_rct()` returns an object of class "wk_grd_rct".
#'   - `grd_xy()` returns an object of class "wk_grd_xy".
#' @export
#'
#' @examples
#' # create a grid with no data (just for coordinates)
#' (grid <- grd(nx = 2, ny = 2))
#' as_rct(grid)
#' as_xy(grid)
#' plot(grid, border = "black")
#'
#' # more usefully, wraps a matrix or nd array + bbox
#' # approx volcano in New Zealand Transverse Mercator
#' bbox <- rct(
#'   5917000,       1757000 + 870,
#'   5917000 + 610, 1757000,
#'   crs = "EPSG:2193"
#' )
#' (grid <- grd_rct(volcano, bbox))
#'
#' # these come with a reasonable default plot method for matrix data
#' plot(grid)
#'
#' # you can set the data or the bounding box after creation
#' grid$bbox <- rct(0, 0, 1, 1)
#'
#' # subset by indices or rct
#' plot(grid[1:2, 1:2])
#' plot(grid[c(start = NA, stop = NA, step = 2), c(start = NA, stop = NA, step = 2)])
#' plot(grid[rct(0, 0, 0.5, 0.5)])
#'
grd <- function(bbox = NULL, nx = NULL, ny = NULL, dx = NULL, dy = NULL,
                type = c("polygons", "corners", "centers")) {
  if (is.null(bbox)) {
    bbox <- NULL
  } else if (inherits(bbox, "wk_rct")) {
    bbox
  } else {
    wk_bbox(bbox)
  }
  type <- match.arg(type)

  if (is.null(nx) && is.null(ny) && !is.null(dx) && !is.null(dy) && !is.null(bbox)) {
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
  } else if (is.null(dx) && is.null(dy) && !is.null(nx) && !is.null(ny)) {
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
    } else if (type == "corners") {
      nx <- nx + 1
      ny <- ny + 1
    }
  } else {
    stop(
      "Must specify dx, dy, and bbox OR nx and ny.",
      call. = FALSE
    )
  }

  # use a length-zero logical() with correct x and y dims
  data <- array(dim = c(ny, nx, 0))

  if (type == "polygons") {
    grd_rct(data, bbox)
  } else {
    grd_xy(data, bbox)
  }
}

#' @rdname grd
#' @export
grd_rct <- function(data, bbox = rct(0, 0, dim(data)[2], dim(data)[1])) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)

  # normalize data and bbox so that max > min
  normalized <- grd_internal_normalize(data, bbox)
  data <- normalized[[1]]
  bbox <- normalized[[2]]

  # with zero values, bbox in that direction is empty
  rct <- unclass(bbox)
  if (dim(data)[2] == 0) {
    rct$xmin <- Inf
    rct$xmax <- -Inf
  }

  if (dim(data)[1] == 0) {
    rct$ymin <- Inf
    rct$ymax <- -Inf
  }

  bbox <- new_wk_rct(rct, crs = wk_crs(bbox))

  new_wk_grd(list(data = data, bbox = bbox), "wk_grd_rct")
}

#' @rdname grd
#' @export
grd_xy <- function(data, bbox = rct(0, 0, dim(data)[2] - 1, dim(data)[1] - 1)) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)

  # normalize data and bbox so that max > min
  normalized <- grd_internal_normalize(data, bbox)
  data <- normalized[[1]]
  bbox <- normalized[[2]]

  # with zero values, bbox in that direction is empty
  rct <- unclass(bbox)
  if (dim(data)[2] == 0) {
    rct$xmin <- Inf
    rct$xmax <- -Inf
  }

  if (dim(data)[1] == 0) {
    rct$ymin <- Inf
    rct$ymax <- -Inf
  }

  bbox <- new_wk_rct(rct, crs = wk_crs(bbox))

  # with one value in the x dimension, we need a zero width bbox
  if (dim(data)[2] == 1) {
    stopifnot(
      unclass(bbox)$xmax == unclass(bbox)$xmin
    )
  }

  # with one value in the y dimension, we need a zero height bbox
  if (dim(data)[1] == 1) {
    stopifnot(
      unclass(bbox)$ymax == unclass(bbox)$ymin
    )
  }

  new_wk_grd(list(data = data, bbox = bbox), "wk_grd_xy")
}

#' @rdname grd
#' @export
as_grd_rct <- function(x, ...) {
  UseMethod("as_grd_rct")
}

#' @rdname grd
#' @export
as_grd_rct.wk_grd_rct <- function(x, ...) {
  x
}

#' @rdname grd
#' @export
as_grd_rct.wk_grd_xy <- function(x, ...) {
  # from a grd_xy, we assume these were the centres
  s <- grd_summary(x)

  bbox <- rct(
    s$xmin - s$dx / 2,
    s$ymin - s$dy / 2,
    s$xmax + s$dx / 2,
    s$ymax + s$dy / 2,
    crs = wk_crs(x$bbox)
  )

  grd_rct(x$data, bbox = bbox)
}

#' @rdname grd
#' @export
as_grd_xy <- function(x, ...) {
  UseMethod("as_grd_xy")
}

#' @rdname grd
#' @export
as_grd_xy.wk_grd_xy <- function(x, ...) {
  x
}

#' @rdname grd
#' @export
as_grd_xy.wk_grd_rct <- function(x, ...) {
  # from a grid_rct() we take the centers
  s <- grd_summary(x)

  bbox <- rct(
    s$xmin + s$dx / 2,
    s$ymin + s$dy / 2,
    s$xmax - s$dx / 2,
    s$ymax - s$dy / 2,
    crs = wk_crs(x$bbox)
  )

  grd_xy(x$data, bbox = bbox)
}

#' S3 details for grid objects
#'
#' @param x A [grd()]
#' @param subclass An optional subclass.
#'
#' @return An object inheriting from 'grd'
#'
#' @export
#'
new_wk_grd <- function(x, subclass = character()) {
  structure(x, class = union(subclass, "wk_grd"))
}


#' Grid information
#'
#' @param grid A [grd_xy()], [grd_rct()], or other object
#'   implementing `grd_*()` methods.
#' @return
#'   - `grd_summary()` returns a `list()` with components
#'     `xmin`, `ymin`, `xmax`, `ymax`,
#'     `nx`, `ny`, `dx`, `dy`, `width`, and `height`.
#' @export
#'
#' @examples
#' grd_summary(grd(nx = 3, ny = 2))
#'
grd_summary <- function(grid) {
  UseMethod("grd_summary")
}

#' @export
grd_summary.wk_grd_rct <- function(grid) {
  nx <- dim(grid$data)[2]
  ny <- dim(grid$data)[1]
  rct <- unclass(grid$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- width / nx
  dy <- height / ny

  list(
    xmin = rct$xmin,
    ymin = rct$ymin,
    xmax = rct$xmax,
    ymax = rct$ymax,
    nx = nx,
    ny = ny,
    dx = dx,
    dy = dy,
    width = width,
    height = height
  )
}

#' @export
grd_summary.wk_grd_xy <- function(grid) {
  nx <- dim(grid$data)[2]
  ny <- dim(grid$data)[1]
  rct <- unclass(grid$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- if (nx > 1) width / (nx - 1) else 0
  dy <- if (ny > 1) height / (ny - 1) else 0

  list(
    xmin = rct$xmin,
    ymin = rct$ymin,
    xmax = rct$xmax,
    ymax = rct$ymax,
    nx = nx,
    ny = ny,
    dx = dx,
    dy = dy,
    width = width,
    height = height
  )
}

# interface for wk methods

#' @export
wk_bbox.wk_grd <- function(handleable, ...) {
  handleable$bbox
}

#' @export
wk_crs.wk_grd <- function(x) {
  attr(x$bbox, "crs", exact = TRUE)
}

#' @export
wk_set_crs.wk_grd <- function(x, crs) {
  x$bbox <- wk_set_crs(x$bbox, crs)
  x
}

# interface for setting data and bbox

#' @export
`[[<-.wk_grd` <- function(x, i, value) {
  x_bare <- unclass(x)
  if (identical(i, "data")) {
    stopifnot(length(dim(value)) >= 2)
    x_bare$data <- value
  } else if (identical(i, "bbox")) {
    if (inherits(value, "wk_rct")) {
      # normalize so that max > min, but empty (Inf -Inf) is OK
      rct <- unclass(value)

      # missings make no sense in this context
      if (any(vapply(rct, is.na, logical(1)))) {
        stop("Can't set missing bounding box for grd objects", call. = FALSE)
      }

      if ((rct$xmin > rct$xmax) && (rct$xmin != Inf) && (rct$xmax != -Inf)) {
        rct[c("xmin", "xmax")] <- rct[c("xmax", "xmin")]
      }
      if ((rct$ymin > rct$ymax) && (rct$ymin != Inf) && (rct$ymax != -Inf)) {
        rct[c("ymin", "ymax")] <- rct[c("ymax", "ymin")]
      }

      value <- new_wk_rct(rct, crs = wk_crs(value))
    } else {
      value <- wk_bbox(value)
    }

    x_bare$bbox <- value
  } else {
    stop("Can't set element of a grd that is not 'data' or 'bbox'", call. = FALSE)
  }

  class(x_bare) <- class(x)
  x_bare
}

#' @export
`$<-.wk_grd` <- function(x, i, value) {
  x[[i]] <- value
  x
}

# interface for matrix-like extraction and subsetting
#' @export
dim.wk_grd <- function(x) {
  dim(x$data)
}

#' @export
`[.wk_grd` <- function(x, i, j, ..., drop = FALSE) {
  # for this method we never drop dimensions (can use $data[] to do this)
  stopifnot(identical(drop, FALSE))

  bbox <- NULL

  if (missing(i)) {
    i <- NULL
  }

  if (missing(j)) {
    j <- NULL
  }

  # allow combination of i, j to be a rct() instead
  if (inherits(i, "wk_rct") && is.null(j)) {
    result_xy <- grd_crop(x, bbox = i)
    if (length(dim(x$data)) > 2) {
      result_xy$data <- result_xy$data[, , , ..., drop = FALSE]
    } else {
      result_xy$data <- result_xy$data[, , ..., drop = FALSE]
    }
  } else if (inherits(i, "wk_rct")) {
    result_xy <- grd_crop(x, bbox = i)
    result_xy$data <- result_xy$data[, , j, ..., drop = FALSE]
  } else {
    result_xy <- grd_subset(x, i = i, j = j)
    result_xy$data <- result_xy$data[, , ..., drop = FALSE]
  }

  result_xy
}

#' @export
format.wk_grd <- function(x, ...) {
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
print.wk_grd <- function(x, ...) {
  cat(paste0(format(x), "\n"))
  utils::str(x)
  invisible(x)
}

# normalize the data and the bbox such that xmax > xmin and ymax > ymin
grd_internal_normalize <- function(x, bbox) {
  rct <- unclass(bbox)
  new_rct <- rct

  if ((rct$ymin > rct$ymax) && (dim(x)[1] > 0)) {
    new_rct$ymin <- rct$ymax
    new_rct$ymax <- rct$ymin

    if (inherits(x, "nativeRaster")) {
      # the dimensions of a nativeRaster are lying in the sense that
      # they are row-major but are being stored column-major in the way
      # that R's indexing functions work
      attrs <- attributes(x)
      dim(x) <- rev(dim(x))
      x <- x[, ncol(x):1, drop = FALSE]
      attributes(x) <- attrs
    } else {
      x <- x[nrow(x):1, , drop = FALSE]
    }
  }

  if ((rct$xmin > rct$xmax) && (dim(x)[2] > 0)) {
    new_rct$xmin <- rct$xmax
    new_rct$xmax <- rct$xmin

    if (inherits(x, "nativeRaster")) {
      attrs <- attributes(x)
      dim(x) <- rev(dim(x))
      x <- x[nrow(x):1, , drop = FALSE]
      attributes(x) <- attrs
    } else {
      x <- x[, ncol(x):1, drop = FALSE]
    }
  }

  list(x = x, bbox = new_wk_rct(new_rct, crs = wk_crs(bbox)))
}
