
#' Subset grd objects
#'
#' @param object A [grd()]
#' @param x Indices in the x direction
#' @param y Indices in the y direction
#' @param bbox A bounding box to use as a subset
#' @param ... Passed to subset methods
#'
#' @return A modified [grd()]
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_subset(grid, seq(2, 61, by = 4), seq(2, 87, by = 4))
#'
grd_subset <- function(object, y = NULL, x = NULL, bbox = NULL, ...) {
  UseMethod("grd_subset")
}

#' @export
grd_subset.wk_grd_rct <- function(object, y = NULL, x = NULL, bbox = NULL, ...) {
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
    if (is.null(x)) {
      x <- seq_len(nx)
    } else if (is.numeric(x)) {
      stopifnot(length(x) <= 1 || length(unique(diff(x))) == 1)
    } else if (is.logical(y)) {
      x <- which(rep_len(x, nx))
    } else {
      stop("`x` must be NULL, numeric, or logical", call. = FALSE)
    }

    if (is.null(y)) {
      y <- seq_len(ny)
    } else if (is.numeric(y)) {
      stopifnot(length(y) <= 1 || length(unique(diff(y))) == 1)
    } else if (is.logical(y)) {
      y <- which(rep_len(y, ny))
    } else {
      stop("`y` must be NULL, numeric, or logical", call. = FALSE)
    }
  } else if (!is.null(bbox) && is.null(x) && is.null(y)) {
    # normalized so that xmin < xmax, ymin < ymax
    rct_target <- unclass(if (inherits(bbox, "wk_rct")) wk_bbox(as_wkb(bbox)) else wk_bbox(bbox))

    # clamp to current limits
    rct_target$xmin <- max(rct_target$xmin, rct$xmin)
    rct_target$ymin <- max(rct_target$ymin, rct$ymin)
    rct_target$xmax <- min(rct_target$xmax, rct$xmax)
    rct_target$ymax <- min(rct_target$ymax, rct$ymax)

    # remember that y indices are upside down compared to limits
    ximin <- (rct_target$xmin - rct$xmin) / dx
    yimin <- (rct$ymax - rct_target$ymax) / dy
    ximax <- ximin + (rct_target$xmax - rct_target$xmin) / dx
    yimax <- yimin + (rct_target$ymax - rct_target$ymin) / dy

    # this subset will get us intersecting cells but NOT cells
    # that only touch on the bottom/right
    if (ceiling(ximax) != ximax) {
      ximax <- ceiling(ximax)
    }

    if (ceiling(yimax) != yimax) {
      yimax <- ceiling(yimax)
    }

    x <- seq(floor(ximin) + 1L, ximax)
    y <- seq(floor(yimin) + 1L, yimax)
  } else {
    stop("Must specify bbox OR (x | y)", call. = FALSE)
  }

  # clamp to actual indices
  x <- x[!is.na(x) & (x >= 1L) & (x <= nx)]
  y <- y[!is.na(y) & (y >= 1L) & (y <= ny)]

  # sort to avoid flipping any data
  x <- sort(x)
  y <- sort(y)

  # re-define the bbox based on actual indices
  # strategy is to keep the cell centres intact,
  # which may change the bbox if downsampling
  new_rct <- rct

  if (length(x) > 0) {
    downsample_x <- x[2] - x[1]
    new_dx <- if (length(x) >= 2) dx * downsample_x else dx
    new_rct$xmin <- rct$xmin + ((min(x) - 1) * dx) + (dx / 2) - (new_dx / 2)
    new_rct$xmax <- rct$xmin + (max(x) * dx) - (dx / 2) + (new_dx / 2)
  } else {
    new_rct$xmin <- Inf
    new_rct$xmax <- -Inf
  }

  if (length(y) > 0) {
    downsample_y <- y[2] - y[1]
    new_dy <- if (length(y) >= 2) dy * downsample_y else dy
    new_rct$ymin <- rct$ymax - (max(y) * dy) + (dy / 2) - (new_dy / 2)
    new_rct$ymax <- rct$ymax - ((min(y) - 1) * dy) - (dy / 2) + (new_dy / 2)
  } else {
    new_rct$ymin <- Inf
    new_rct$ymax <- -Inf
  }

  # about to potentially modify data
  data <- object$data

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  if (inherits(data, "nativeRaster")) {
    attrs <- attributes(data)
    dim(data) <- rev(dim(data))
    data <- data[x, y, drop = FALSE]
    attrs$dim <- rev(dim(data))
    attributes(data) <- attrs
  } else {
    # we want to keep everything for existing dimensions
    # this means generating a list of missings to fill
    # the correct number of additional dimensions
    n_more_dims <- length(dim(data)) - 2L
    more_dims <- alist(1, )[rep(2, n_more_dims)]
    data <- do.call("[", c(list(data, y, x), more_dims, list(drop = FALSE)))
  }

  grd_rct(
    data,
    bbox = new_wk_rct(new_rct, crs = wk_crs(object)),
    data_order = object$data_order
  )
}
