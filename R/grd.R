
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
#' plot(grid[c(FALSE, TRUE, FALSE), c(FALSE, TRUE, FALSE)])
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
  data_order <- grd_data_order(data)

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

  new_wk_grd(list(data = data, bbox = bbox, data_order = data_order), "wk_grd_rct")
}

#' @rdname grd
#' @export
grd_xy <- function(data, bbox = rct(0, 0, dim(data)[2] - 1, dim(data)[1] - 1)) {
  bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  data_order <- grd_data_order(data)

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

  new_wk_grd(list(data = data, bbox = bbox, data_order = data_order), "wk_grd_xy")
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

  grd <- grd_rct(x$data, bbox = bbox)
  grd$data_order <- x$data_order
  grd
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

  grd <- grd_xy(x$data, bbox = bbox)
  grd$data_order <- x$data_order
  grd
}

#' S3 details for grid objects
#'
#' @param x A [grd()]
#' @param subclass An optional subclass.
#' @param ... Passed to S3 methods
#'
#' @export
#'
new_wk_grd <- function(x, subclass = character()) {
  structure(x, class = union(subclass, "wk_grd"))
}

#' @rdname new_wk_grd
#' @export
grd_data_order <- function(x, ...) {
  UseMethod("grd_data_order")
}

#' @export
grd_data_order.default <- function(x, ...) {
  c("y", "x")
}

#' @export
grd_data_order.nativeRaster <- function(x, ...) {
  c("x", "y")
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

      if (is.na(rct$xmin) || is.na(rct$xmax)) {
        rct[c("xmin", "xmax")] <- list(Inf, -Inf)
      }
      if (is.na(rct$ymin) || is.na(rct$ymax)) {
        rct[c("ymin", "ymax")] <- list(Inf, -Inf)
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
  } else if(identical(i, "data_order")) {
    if (setequal(gsub("^[+-]", "", value), c("x", "y"))) {
      x_bare$data_order <- value
    } else {
      stop(
        "element 'data_order' must be `c(\"[-]y\", \"[-]x\")` or `c(\"[-]x\", \"[-]y\")`",
        call. = FALSE
      )
    }
  } else {
    stop("Can't set element of a wk_grd that is not 'data' or 'bbox'", call. = FALSE)
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
  dims <- dim(x$data)
  names(dims)[1:2] <- c("y", "x")
  dims
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
    result_xy <- grd_subset(x, bbox = i)
    if (length(dim(x$data)) > 2) {
      result_xy$data <- result_xy$data[, , , ..., drop = FALSE]
    } else {
      result_xy$data <- result_xy$data[, , ..., drop = FALSE]
    }
  } else if (inherits(i, "wk_rct")) {
    result_xy <- grd_subset(x, bbox = i)
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

  xs <- seq(rct$xmin, rct$xmax, by = width / (nx - 1))
  ys <- seq(rct$ymax, rct$ymin, by = -height / (ny - 1))

  # ordering such that values match up to internal data ordering
  data_order <- gsub("^[+-]", "", x$data_order)

  if (identical(data_order, c("y", "x"))) {
    if (startsWith("-", x$data_order[1])) {
      ys <- rev(ys)
    }

    if (startsWith("-", x$data_order[2])) {
      xs <- rev(xs)
    }

    xy(
      rep(xs, each = length(ys)),
      rep(ys, length(xs)),
      crs = wk_crs(x$bbox)
    )
  } else {
    if (startsWith("-", x$data_order[2])) {
      ys <- rev(ys)
    }

    if (startsWith("-", x$data_order[1])) {
      xs <- rev(xs)
    }

    xy(
      rep(xs, length(ys)),
      rep(ys, each = length(xs)),
      crs = wk_crs(x$bbox)
    )
  }
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

  # ordering such that values match up to internal data ordering
  xs <- seq(rct$xmin, rct$xmax, by = width / nx)
  ys <- seq(rct$ymax, rct$ymin, by = -height / ny)

  data_order <- gsub("^[+-]", "", x$data_order)

  if (identical(data_order, c("y", "x"))) {
    if (startsWith("-", x$data_order[1])) {
      ys <- rev(ys)
      ymax <- rep(ys[-1], nx)
      ymin <- rep(ys[-length(ys)], nx)
    } else {
      ymin <- rep(ys[-1], nx)
      ymax <- rep(ys[-length(ys)], nx)
    }

    if (startsWith("-", x$data_order[2])) {
      xs <- rev(xs)
      xmax <- rep(xs[-length(xs)], each = ny)
      xmin <- rep(xs[-1], each = ny)
    } else {
      xmin <- rep(xs[-length(xs)], each = ny)
      xmax <- rep(xs[-1], each = ny)
    }

    rct(xmin, ymin, xmax, ymax, crs = wk_crs(x$bbox))
  } else {
    if (startsWith("-", x$data_order[2])) {
      ys <- rev(ys)
      ymax <- rep(ys[-1], each = nx)
      ymin <- rep(ys[-length(ys)], each = nx)
    } else {
      ymin <- rep(ys[-1], each = nx)
      ymax <- rep(ys[-length(ys)], each = nx)
    }

    if (startsWith("-", x$data_order[1])) {
      xs <- rev(xs)
      xmax <- rep(xs[-length(xs)], ny)
      xmin <- rep(xs[-1], ny)
    } else {
      xmin <- rep(xs[-length(xs)], ny)
      xmax <- rep(xs[-1], ny)
    }
  }

  rct(xmin, ymin, xmax, ymax, crs = wk_crs(x$bbox))
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
    (is.na(dim(x$data)[3]) || (dim(x$data)[3] <= 1))

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
      image <- matrix(nrow = dim(x$data)[1], ncol = dim(x$data)[2])
    }

    as.raster(image)
  } else {
    as.raster(x$data, native = native)
  }
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
