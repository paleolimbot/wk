
#' Subset grd objects
#'
#' The [grd_subset()] method handles the subsetting of a [grd()]
#' in x-y space. Ordering of indices is not considered and logical
#' indies are recycled silently along dimensions. The result of
#' a [grd_subset()] is always a [grd()] of the same type whose
#' relationship to x-y space has not changed.
#'
#' @param object A [grd()]
#' @param i,j Raw indices. These must be equally
#'   spaced if passed as numeric; if passed as logical they are
#'   recycled silently along each dimension.
#' @param bbox A bounding box to use as a subset. This is used
#'   to calculate a suitable `y` and `x` index vector representing
#'   all cells that intersect the `bbox`. Cells that only touch `bbox`
#'   on the bottom and right are not included in the subset, meaning you
#'   can safely tile a regularly-spaced grid along `object` without
#'   double-counting cells.
#' @param ... Passed to subset methods
#'
#' @return A modified [grd()].
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_subset(grid, seq(2, 61, by = 4), seq(2, 87, by = 4))
#'
grd_subset <- function(object, i = NULL, j = NULL, bbox = NULL, ...) {
  UseMethod("grd_subset")
}

#' @export
grd_subset.default <- function(object, i = NULL, j = NULL, bbox = NULL, ...) {
  indices <- grd_subset_indices(object, i, j, bbox, ...)
  x <- indices$j
  y <- indices$i

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

  object$data <- data
  object$bbox <- indices$bbox
  object
}

#' @rdname grd_subset
#' @export
grd_subset_indices <- function(object, i = NULL, j = NULL, bbox = NULL, ...) {
  UseMethod("grd_subset_indices")
}

#' @export
grd_subset_indices.wk_grd_xy <- function(object, i = NULL, j = NULL, bbox = NULL, ...) {
  grd_subset_indices_internal(object, i, j, bbox)
}

#' @export
grd_subset_indices.wk_grd_rct <- function(object, i = NULL, j = NULL, bbox = NULL, ...) {
  grd_subset_indices_internal(object, i, j, bbox)
}

grd_subset_indices_internal <- function(object, i = NULL, j = NULL, bbox = NULL) {
  # get the cell information we need
  rct <- unclass(object$bbox)
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin
  nx <- dim(object$data)[2]
  ny <- dim(object$data)[1]

  if (inherits(object, "wk_grd_rct")) {
    dx <- width / nx
    dy <- height / ny
  } else if (inherits(object, "wk_grd_xy")) {
    dx <- width / (nx - 1)
    dy <- height / (ny- 1)
  } else {
    stop("Unknown object type", call. = FALSE) # nocov
  }

  # can't get any more subsetted than an empty grid!
  if ((nx * ny) == 0) {
    return(
      list(
        i = seq_len(nrow(object)),
        j = seq_len(ncol(object)),
        bbox = object$bbox
      )
    )
  }

  # empty subset criteria -> no subset
  if (is.null(bbox) && is.null(i) && is.null(j)) {
    return(
      list(
        i = seq_len(nrow(object)),
        j = seq_len(ncol(object)),
        bbox = object$bbox
      )
    )
  }

  if (is.null(bbox) && (!is.null(i) || !is.null(j))) {
    indices <- grd_expand_ij_internal(i, j, nx, ny)
  } else if (!is.null(bbox) && is.null(i) && is.null(j)) {
    indices <- grd_expand_bbox_rct_internal(object, bbox, dx, dy)
  } else {
    stop("Must specify bbox OR (i | j)", call. = FALSE)
  }

  x <- indices$j
  y <- indices$i

  # clamp to actual indices
  x <- x[!is.na(x) & (x >= 1L) & (x <= nx)]
  y <- y[!is.na(y) & (y >= 1L) & (y <= ny)]

  # sort to avoid flipping any data
  x <- sort(x)
  y <- sort(y)

  # re-define the bbox based on actual index subset
  new_rct <- rct

  if (inherits(object, "wk_grd_rct")) {
    # strategy is to keep the cell centres intact on downsample
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
  } else if (inherits(object, "wk_grd_xy")) {
    if (length(x) > 0) {
      new_rct$xmin <- rct$xmin + (x[1] - 1L) * dx
      new_rct$xmax <- rct$xmin + (x[length(x)] - 1L) * dx
    } else {
      new_rct$xmin <- Inf
      new_rct$xmax <- -Inf
    }

    if (length(y) > 0) {
      new_rct$ymin <- rct$ymax - (y[1] - 1L) * dy
      new_rct$ymax <- rct$ymax - (y[length(y)] - 1L) * dy
    } else {
      new_rct$ymin <- Inf
      new_rct$ymax <- -Inf
    }
  } else {
    stop("Unknown object type", call. = FALSE) # nocov
  }

  list(i = y, j = x, bbox = new_wk_rct(new_rct, crs = wk_crs(object)))
}

grd_expand_bbox_rct_internal <- function(object, bbox_target, dx, dy) {
  # for access to members
  rct <- unclass(object$bbox)

  # normalized so that xmin < xmax, ymin < ymax
  if (inherits(bbox_target, "wk_rct")) {
    rct_target <- unclass(wk_bbox(as_wkb(bbox_target)))
  } else {
    rct_target <- unclass(wk_bbox(bbox_target))
  }

  # clamp to current limits
  rct_target$xmin <- max(rct_target$xmin, rct$xmin)
  rct_target$ymin <- max(rct_target$ymin, rct$ymin)
  rct_target$xmax <- min(rct_target$xmax, rct$xmax)
  rct_target$ymax <- min(rct_target$ymax, rct$ymax)

  # It might be possible here to have a non-intersecting rct_target
  # in which case we can return an empty subset.
  # Notably, rct(Inf, Inf, -Inf, -Inf)
  if ((rct_target$xmin > rct_target$xmax) || (rct_target$ymin > rct_target$ymax)) {
    return(list(i = integer(), j = integer()))
  }

  # remember that y indices are upside down compared to limits
  ximin <- (rct_target$xmin - rct$xmin) / dx
  yimin <- (rct$ymax - rct_target$ymax) / dy
  ximax <- ximin + (rct_target$xmax - rct_target$xmin) / dx
  yimax <- yimin + (rct_target$ymax - rct_target$ymin) / dy

  if (inherits(object, "wk_grd_rct")) {
    # this subset will get us intersecting cells but NOT cells
    # that only touch on the bottom/right
    if (ceiling(ximax) != ximax) {
      ximax <- ceiling(ximax)
    }

    if (ceiling(yimax) != yimax) {
      yimax <- ceiling(yimax)
    }

    yimin <- floor(yimin) + 1L
    ximin <- floor(ximin) + 1L
  } else if (inherits(object, "wk_grd_xy")) {
    # this subset gets us any point that intersects the bbox
    # including the boundary on all sides
    yimin <- ceiling(yimin + 1L)
    yimax <- floor(yimax) + 1L
    ximin <- ceiling(ximin + 1L)
    ximax <- floor(ximax) + 1L
  } else {
    stop("Unknown containment option", call. = FALSE) # nocov
  }

  list(
    i = if (yimax >= yimin) seq(yimin, yimax) else integer(),
    j = if (ximax >= ximin) seq(ximin, ximax) else integer()
  )
}

grd_expand_ij_internal <- function(i, j, nx, ny) {
  if (is.null(j)) {
    j <- seq_len(nx)
  } else if (is.numeric(j)) {
    stopifnot(length(j) <= 1 || length(unique(diff(j))) == 1)
  } else if (is.logical(j)) {
    j <- which(rep_len(j, nx))
    stopifnot(length(j) <= 1 || length(unique(diff(j))) == 1)
  } else {
    stop("`j` must be NULL, numeric, or logical", call. = FALSE)
  }

  if (is.null(i)) {
    i <- seq_len(ny)
  } else if (is.numeric(i)) {
    stopifnot(length(i) <= 1 || length(unique(diff(i))) == 1)
  } else if (is.logical(i)) {
    i <- which(rep_len(i, ny))
    stopifnot(length(i) <= 1 || length(unique(diff(i))) == 1)
  } else {
    stop("`i` must be NULL, numeric, or logical", call. = FALSE)
  }

  list(i = i, j = j)
}
