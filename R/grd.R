
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
#'   - `grd()` returns a `grd_rct()` for `type == "polygons` or
#'     a `grd_xy()` otherwise.
#'   - `grd_rct()` returns an object of class "grd_rct".
#'   - `grd_xy()` returns an object of class "grd_xy".
#' @export
#'
#' @examples
#' grd_rct(volcano)
#' # approx bounding box in New Zealand Transverse Mercator
#' bbox <- rct(
#'   5917000,       1757000 + 870,
#'   5917000 + 610, 1757000,
#'   crs = "EPSG:2193"
#' )
#' grd_rct(volcano, bbox)
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
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  # with zero values in the xy direction, bbox is empty
  if ((dim(data)[1] * dim(data)[2]) == 0) {
    return(
      new_grd(
        list(
          data = data,
          bbox = wk::wk_bbox(xy(crs = wk_crs(bbox)))
        ),
        "wk_grd_rct"
      )
    )
  }

  new_grd(list(data = data, bbox = bbox), "wk_grd_rct")
}

#' @rdname grd
#' @export
grd_xy <- function(data, bbox = rct(0, 0, dim(data)[2] - 1, dim(data)[1] - 1)) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  stopifnot(
    length(bbox) == 1,
    length(dim(data)) >= 2
  )

  # with zero values in the xy direction, bbox is empty
  if ((dim(data)[1] * dim(data)[2]) == 0) {
    return(
      new_grd(
        list(
          data = data,
          bbox = wk::wk_bbox(xy(crs = wk_crs(bbox)))
        ),
        "wk_grd_xy"
      )
    )
  }

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

  new_grd(list(data = data, bbox = bbox), "wk_grd_xy")
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
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  rct <- unclass(x$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- if (nx > 1) width / (nx - 1) else 0
  dy <- if (ny > 1) height / (ny - 1) else 0

  bbox <- rct(
    rct$xmin - dx / 2,
    rct$ymin - dy / 2,
    rct$xmax + dx / 2,
    rct$ymax + dy / 2,
    crs = wk_crs(x$bbox)
  )

  grd_rct(x$data, bbox)
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
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  rct <- unclass(x$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- width / nx
  dy <- height / ny

  bbox <- rct(
    rct$xmin + dx / 2,
    rct$ymin + dy / 2,
    rct$xmax - dx / 2,
    rct$ymax - dy / 2,
    crs = wk_crs(x$bbox)
  )

  grd_xy(x$data, bbox)
}

#' S3 details for grid objects
#'
#' @param x A [grd()]
#' @param subclass An optional subclass.
#'
#' @export
#'
new_grd <- function(x, subclass = character()) {
  structure(x, class = union(subclass, "wk_grd"))
}

#' @export
wk_bbox.wk_grd <- function(handleable, ...) {
  # take the bbox of the bbox to normalize a bounding box
  # with xmin > xmax
  wk_bbox(as_wkb(handleable$bbox))
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

#' @export
as_xy.wk_grd_xy <- function(x, ...) {
  rct <- unclass(x$bbox)
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin

  if (identical(width, -Inf) || identical(height, -Inf)) {
    return(xy(crs = wk_crs(x)))
  }

  # ordering such that values match up
  # when dim(x$data) <- NULL (nativeRaster maybe should be
  # special-cased or maybe there needs to be an order specified
  # in the constructor
  xs <- seq(rct$xmin, rct$xmax, by = width / (nx - 1))
  ys <- seq(rct$ymax, rct$ymin, by = -height / (ny - 1))
  xy(
    rep(xs, each = length(ys)),
    rep(ys, length(xs))
  )
}

#' @export
as_rct.wk_grd_rct <- function(x, ...) {
  rct <- unclass(x$bbox)
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin

  if (identical(width, -Inf) || identical(height, -Inf)) {
    return(rct(crs = wk_crs(x)))
  }

  # ordering such that values match up
  # when dim(x$data) <- NULL (nativeRaster maybe should be
  # special-cased or maybe there needs to be an order specified
  # in the constructor
  xs <- seq(rct$xmin, rct$xmax, by = width / nx)
  ys <- seq(rct$ymax, rct$ymin, by = -height / ny)

  rct(
    rep(xs[-length(xs)], each = ny),
    rep(ys[-1], nx),
    rep(xs[-1], each = ny),
    rep(ys[-length(ys)], nx)
  )
}

#' @export
as_xy.wk_grd_rct <- function(x, ...) {
  as_xy(as_grd_xy(x))
}

#' @export
as_rct.wk_grd_xy <- function(x, ...) {
  as_rct(as_grd_rct(x))
}

#' @export
#' @importFrom grDevices as.raster
as.raster.wk_grd_rct <- function(x, ..., native = NA) {
  # as.raster() works when values are [0..1]. We can emulate
  # this default by rescaling the image data if it's not already
  # a raster or nativeRaster.
  if (inherits(x$data, "nativeRaster") || inherits(x$data, "raster")) {
    x$data
  } else if (length(setdiff(class(x$data), c("matrix", "array"))) == 0) {
    range <- suppressWarnings(range(x$data, finite = TRUE))
    if (all(is.finite(range)) && (diff(range) > .Machine$double.eps)) {
      image <- (x$data - range[1]) / diff(range)
    } else if (all(is.finite(range))) {
      # constant value
      image <- x$data
      image[] <- 0.5
    } else {
      # all NA values or zero-length (likely for a grd())
      image <- matrix(nrow = dim(x$data)[1], ncol = dim(x$data)[2])
    }

    as.raster(image)
  } else {
    as.raster(x$data, native = native)
  }
}
