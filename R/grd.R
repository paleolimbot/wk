
#' Raster-like objects
#'
#' @param data An object with two or more dimensions. Most usefully, a matrix.
#' @param bbox A [rct()] containing the bounds and CRS of the object.
#' @param data_order The order in which the grid should be iterated to match
#'   the internal ordering of `data`. This is used for objects like nativeRaster
#'   whose internal data ordering is row-major.
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
                    data_order = c("x", "y")) {
  stopifnot(setequal(data_order, c("x", "y")))
  if (is.null(bbox)) {
    bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  }

  dims <- c("x" = dim(data)[2], "y" = dim(data)[1])

  # with zero values in the xy direction, bbox is empty
  if ((dims["x"] * dims["y"]) == 0) {
    bbox <- wk_bbox(xy(crs = wk_crs(bbox)))
  }

  new_grd(list(data = data, bbox = bbox, data_order = data_order), "wk_grd_rct")
}

#' @rdname grd
#' @export
grd_xy <- function(data, bbox = rct(0, 0, dim(data)[2] - 1, dim(data)[1] - 1),
                   data_order = c("x", "y")) {
  stopifnot(setequal(data_order, c("x", "y")))
  if (is.null(bbox)) {
    bbox <- if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox)
  }

  dims <- c("x" = dim(data)[2], "y" = dim(data)[1])

  # with zero values in the xy direction, bbox is empty
  if ((dims["x"] * dims["y"]) == 0) {
    bbox <- wk_bbox(xy(crs = wk_crs(bbox)))
  }

  # with one value in the x dimension, we need a zero width bbox
  if (dims["x"] == 1) {
    stopifnot(
      unclass(bbox)$xmax == unclass(bbox)$xmin
    )
  }

  # with one value in the y dimension, we need a zero height bbox
  if (dims["y"] == 1) {
    stopifnot(
      unclass(bbox)$ymax == unclass(bbox)$ymin
    )
  }

  new_grd(list(data = data, bbox = bbox, data_order = data_order), "wk_grd_xy")
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


#' Subset grd objects
#'
#' @param object A [grd()]
#' @param x Indices in the x direction
#' @param y Indices in the y direction
#' @param ... Pased to the subset method
#' @param bbox A bounding box to use as a subset
#'
#' @return A modified [grd()]
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_subset(grid, seq(2, 61, by = 4), seq(2, 87, by = 4))
#'
grd_subset <- function(object, x = NULL, y = NULL, ..., bbox = NULL) {
  UseMethod("grd_subset")
}

#' @export
grd_subset.wk_grd_rct <- function(object, x = NULL, y = NULL, ..., bbox = NULL) {
  if (missing(x)) {
    x <- NULL
  }

  if (missing(y)) {
    y <- NULL
  }

  # get the cell information we need
  rct <- unclass(object$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  nx <- dim(object$data)[2]
  ny <- dim(object$data)[1]
  dx <- width / nx
  dy <- height / ny

  # can't get any more subsetted than an empty grid!
  if ((nx * ny) == 0) {
    return(object)
  }

  # empty subset criteria -> no subset
  if (is.null(bbox) && is.null(x) && is.null(y)) {
    return(object)
  }

  if (is.null(bbox) && (!is.null(x) || !is.null(y))) {
    # x and y must be NULL or specified by seq()
    if (is.null(x)) {
      x <- seq_len(nx)
    } else {
      stopifnot(length(unique(diff(x))) == 1)
    }

    if (is.null(y)) {
      y <- seq_len(ny)
    } else {
      stopifnot(length(unique(diff(y))) == 1)
    }
  } else if (!is.null(bbox) && is.null(x) && is.null(y)) {
    rct_target <- unclass(if (inherits(bbox, "wk_rct")) bbox else wk_bbox(bbox))

    ximin <- (rct_target$xmin - rct$xmin) / dx
    yimin <- (rct_target$ymin - rct$ymin) / dy
    ximax <- ximin + (rct_target$xmax - rct_target$xmin) / dx
    yimax <- yimin + (rct_target$ymax - rct_target$ymin) / dy

    # this subset will get us intersecting cells (should consider
    # if returning a unique subset for the case of tiling along
    # this raster)
    x <- seq(floor(ximin), ceiling(ximax)) + 1L
    y <- seq(floor(yimin), ceiling(yimax)) + 1L
  } else {
    stop("Must specify bbox OR (x | y)", call. = FALSE)
  }

  # clamp to actual indices
  x <- x[!is.na(x) & (x >= 1L) & (x <= nx)]
  y <- y[!is.na(y) & (y >= 1L) & (y <= ny)]

  # we want these sorted such that we get a bbox that
  # properly has xmax > xmin and ymax > ymin
  x <- sort(x, decreasing = width < 0)
  y <- sort(y, decreasing = height < 0)
  rct[c("xmin", "xmax")] <- as.list(range(c(rct$xmin, rct$xmax)))
  rct[c("ymin", "ymax")] <- as.list(range(c(rct$ymin, rct$ymax)))
  dy <- abs(dy)
  dx <- abs(dx)
  downsample_x <- abs(x[2] - x[1])
  downsample_y <- abs(y[2] - y[1])

  # re-define the bbox based on actual indices
  # strategy is to keep the cell centres intact,
  # which may change the bbox if downsampling
  new_dx <- dx * downsample_x
  new_dy <- dy * downsample_y
  new_rct <- rct
  new_rct$xmin <- rct$xmin + (min(x) - 1) * dx + (dx / 2) - (new_dx / 2)
  new_rct$xmax <- rct$xmin + (max(x)) * dx - (dx / 2) + (new_dx / 2)
  new_rct$ymin <- rct$ymin + (min(y) - 1) * dy + (dy / 2) - (new_dy / 2)
  new_rct$ymax <- rct$ymin + (max(y)) * dy - (dy / 2) + (new_dy / 2)

  # about to potentially modify these
  data <- object$data

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  if (inherits(data, "nativeRaster")) {
    dim(data) <- rev(dim(data))
    data[x, y, drop = FALSE]
    dim(data) <- rev(dim(data))
  } else {
    data <- data[y, x, ..., drop = FALSE]
  }

  grd_rct(
    data,
    bbox = new_wk_rct(new_rct, crs = wk_crs(object)),
    data_order = object$data_order
  )
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

  grd_rct(x$data, bbox = bbox, data_order = x$data_order)
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

  grd_xy(x$data, bbox = bbox, data_order = x$data_order)
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
    rep(ys, length(xs)),
    crs = wk_crs(x$bbox)
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
    rep(ys[-length(ys)], nx),
    crs = wk_crs(x$bbox)
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
