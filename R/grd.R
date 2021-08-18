
#' Raster-like objects
#'
#' @param data An object with two or more dimensions. Most usefully, a matrix.
#' @param spec A [grd_data_spec()], used if `data` has a non-standard dimension
#'   ordering or subset method implementation.
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
grd_rct <- function(data, bbox = rct(0, 0, dim(data)[2], dim(data)[1]),
                    spec = grd_data_spec(data)) {
  stopifnot(inherits(spec, "wk_grd_data_spec"))
  if (is.null(spec$bbox)) {
    spec$bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  }

  # with zero values in the xy direction, bbox is empty
  if ((spec$dims["x"] * spec$dims["y"]) == 0) {
    spec$bbox <- wk_bbox(xy(crs = wk_crs(bbox)))
  }

  new_grd(list(data = data, spec = spec), "wk_grd_rct")
}

#' @rdname grd
#' @export
grd_xy <- function(data, bbox = rct(0, 0, dim(data)[2] - 1, dim(data)[1] - 1),
                   spec = grd_data_spec(data)) {
  stopifnot(inherits(spec, "wk_grd_data_spec"))
  if (is.null(spec$bbox)) {
    spec$bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  }

  # with zero values in the xy direction, bbox is empty
  if ((spec$dims["x"] * spec$dims["y"]) == 0) {
    spec$bbox <- wk_bbox(xy(crs = wk_crs(bbox)))
  }

  # with one value in the x dimension, we need a zero width bbox
  if (spec$dims["x"] == 1) {
    stopifnot(
      unclass(bbox)$xmax == unclass(bbox)$xmin
    )
  }

  # with one value in the y dimension, we need a zero height bbox
  if (spec$dims["y"] == 1) {
    stopifnot(
      unclass(bbox)$ymax == unclass(bbox)$ymin
    )
  }

  new_grd(list(data = data, spec = spec), "wk_grd_xy")
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
  nx <- x$spec$dims["x"]
  ny <- x$spec$dims["y"]
  rct <- unclass(x$spec$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- if (nx > 1) width / (nx - 1) else 0
  dy <- if (ny > 1) height / (ny - 1) else 0

  spec <- x$spec
  spec$bbox <- rct(
    rct$xmin - dx / 2,
    rct$ymin - dy / 2,
    rct$xmax + dx / 2,
    rct$ymax + dy / 2,
    crs = wk_crs(spec$bbox)
  )

  grd_rct(x$data, spec = spec)
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
  nx <- x$spec$dims["x"]
  ny <- x$spec$dims["y"]
  rct <- unclass(x$spec$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  dx <- width / nx
  dy <- height / ny

  spec <- x$spec
  spec$bbox <- rct(
    rct$xmin + dx / 2,
    rct$ymin + dy / 2,
    rct$xmax - dx / 2,
    rct$ymax - dy / 2,
    crs = wk_crs(spec$bbox)
  )

  grd_xy(x$data, spec = spec)
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
  wk_bbox(as_wkb(handleable$spec$bbox))
}

#' @export
wk_crs.wk_grd <- function(x) {
  attr(x$spec$bbox, "crs", exact = TRUE)
}

#' @export
wk_set_crs.wk_grd <- function(x, crs) {
  x$spec$bbox <- wk_set_crs(x$spec$bbox, crs)
  x
}

#' @export
format.wk_grd <- function(x, ...) {
  crs <- wk_crs(x)
  sprintf(
    "<%s [%s] => %s%s>",
    class(x)[1],
    paste0(x$spec$dims, collapse = " x "),
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
  rct <- unclass(x$spec$bbox)
  nx <- x$spec$dims["x"]
  ny <- x$spec$dims["y"]
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
    rep(ys, length(xs)),
    crs = wk_crs(x$spec$bbox)
  )
}

#' @export
as_rct.wk_grd_rct <- function(x, ...) {
  rct <- unclass(x$spec$bbox)
  nx <- x$spec$dims["x"]
  ny <- x$spec$dims["y"]
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
    rep(ys[-length(ys)], nx),
    crs = wk_crs(x$spec$bbox)
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
  is_simple_numeric <- (length(setdiff(class(x$data), c("matrix", "array"))) == 0) &&
    !is.character(x$data) &&
    (is.na(x$spec$dims[3]) || (x$spec$dims[3] <= 1))

  if (inherits(x$data, "nativeRaster")) {
    x$data
  } else if (is_simple_numeric) {
    range <- suppressWarnings(range(x$data, finite = TRUE))
    if (all(is.finite(range)) && (diff(range) > .Machine$double.eps)) {
      image <- (x$data - range[1]) / diff(range)
    } else if (all(is.finite(range))) {
      # constant value
      image <- x$data
      image[] <- 0.5
    } else {
      # all NA values or zero-length (likely for a grd())
      image <- matrix(nrow = x$spec$dims["y"], ncol = x$spec$dims["x"])
    }

    as.raster(image)
  } else {
    as.raster(x$data, native = native)
  }
}


#' Grid detail specifications
#'
#' Wraps the details of the `data[]`, `dim(data)`, to allow multiple types
#' of data to back a [grd()]. Notably, this allows a nativeRaster (whose
#' data is defined in row-major order but dimensions report column-major
#' order) to back a [grd()].
#'
#' @param dims The dimensions of the raster
#' @param index_order Mapping of dimensions to index axes.
#'   Default ("y", "x") reflects that of [grDevices::as.raster()]
#'   and [graphics::rasterImage()].
#' @param dim_order Mapping of dimensions to
#' @inheritParams grd
#'
#' @return An object of class "wk_grd_data_spec"
#' @export
#'
#' @examples
#' grd_data_spec(matrix())
#'
new_wk_grd_data_spec <- function(dims, bbox = NULL,
                                 index_order = c("y", "x"),
                                 dim_order = index_order,
                                 data_order = dim_order) {
  stopifnot(
    setequal(index_order, c("x", "y")),
    setequal(dim_order, c("x", "y")),
    setequal(data_order, c("x", "y")),
    is.null(bbox) || (inherits(bbox, "wk_rct") && (length(bbox) == 1)),
    is.integer(dims), length(dims) >= 2
  )

  names(dims)[1:2] <- dim_order

  structure(
    list(
      dims = dims,
      bbox = bbox,
      index_order = index_order,
      dim_order = dim_order
    ),
    class = "wk_grd_data_spec"
  )
}

#' @rdname grd
#' @export
grd_data_spec <- function(data, ...) {
  UseMethod("grd_data_spec")
}

#' @rdname grd
#' @export
grd_data_spec.default <- function(data, ...) {
  new_wk_grd_data_spec(
    dim(data), bbox = NULL,
    index_order = c("y", "x"),
    dim_order = c("y", "x")
  )
}

#' @rdname grd
#' @export
grd_data_spec.nativeRaster <- function(data, ...) {
  new_wk_grd_data_spec(
    dim(data), bbox = NULL,
    index_order = c("x", "y"),
    dim_order = c("y", "x")
  )
}
