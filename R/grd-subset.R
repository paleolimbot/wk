
#' Subset grd grids
#'
#' The [grd_subset()] method handles the subsetting of a [grd()]
#' in x-y space. Ordering of indices is not considered and logical
#' indies are recycled silently along dimensions. The result of
#' a [grd_subset()] is always a [grd()] of the same type whose
#' relationship to x-y space has not changed.
#'
#' @inheritParams grd_summary
#' @param i,j Raw indices. These must be equally
#'   spaced if passed as numeric; if passed as logical they are
#'   recycled silently along each dimension. Indexing grd grids
#'   is always 1-based and always starts from the left and top of
#'   the bounding box regardless of internal data ordering. A
#'   `list()` containing `i` and `j` elements can also be supplied.
#' @param bbox A bounding box to use as a subset. This is used
#'   to calculate a suitable `y` and `x` index vector representing
#'   all cells that intersect the `bbox`. Cells that only touch `bbox`
#'   on the bottom and right are not included in the subset, meaning you
#'   can safely tile a regularly-spaced grid along `grid` without
#'   double-counting cells.
#' @param point A [handleable][wk_handle] of points.
#' @param ... Passed to subset methods
#'
#' @return
#'   - `grd_subset()`: A modified [grd()].
#'   - `grd_subset_indices()`: A `list()` with components
#'     `i` (`c(start = , stop = , step = )`), `j` (`c(start = , stop = , step = )`),
#'     and `bbox` ([rct()] of length 1). In this value `start` is the index before
#'     the first element and `stop` is the last element (equivalent to notation
#'     for a zero-based Python slice).
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_subset(grid, seq(2, 61, by = 4), seq(2, 87, by = 4))
#'
grd_subset <- function(grid, i = NULL, j = NULL, bbox = NULL, ...) {
  UseMethod("grd_subset")
}

#' @rdname grd_subset
#' @export
grd_subset_data <- function(grid, i = NULL, j = NULL, ...) {
  UseMethod("grd_subset")
}

#' @rdname grd_subset
#' @export
grd_crop <- function(grid, bbox, ...) {
  UseMethod("grd_crop")
}

#' @rdname grd_subset
#' @export
grd_extend <- function(grid, bbox, ...) {
  UseMethod("grd_extend")
}

#' @rdname grd_subset
#' @export
grd_index <- function(grid, point, ...) {
  UseMethod("grd_index")
}

#' @rdname grd_subset
#' @export
grd_index_range <- function(grid, bbox, ...) {
  UseMethod("grd_index_range")
}

#' @rdname grd_subset
#' @export
grd_cell <- function(grid, i = NULL, j = NULL, ...) {
  UseMethod("grd_cell")
}

#' @rdname grd_subset
#' @export
grd_center <- function(grid, i = NULL, j = NULL, ...) {
  UseMethod("grd_center")
}

#' @export
grd_subset.default <- function(grid, i = NULL, j = NULL, bbox = NULL, ...) {
  indices <- grd_subset_indices(grid, i, j, bbox, ...)

  if (is.na(indices$i["start"])) {
    indices$i["start"] <- 0L
  }

  if (is.na(indices$i["stop"])) {
    indices$i["stop"] <- unname(dim(grid)[1])
  }

  if (is.na(indices$i["step"])) {
    indices$i["step"] <- 1L
  }

  if (is.na(indices$j["start"])) {
    indices$j["start"] <- 0L
  }

  if (is.na(indices$j["stop"])) {
    indices$j["stop"] <- unname(dim(grid)[2])
  }

  if (is.na(indices$j["step"])) {
    indices$j["step"] <- 1L
  }

  # convert zero-based start/stop/step to vectors of indices
  if (indices$i["start"] >= indices$i["stop"]) {
    i <- integer()
  } else {
    i <- seq(indices$i["start"] + 1L, indices$i["stop"], by = indices$i["step"])
  }

  if (indices$j["start"] >= indices$j["stop"]) {
    j <- integer()
  } else {
    j <- seq(indices$j["start"] + 1L, indices$j["stop"], by = indices$j["step"])
  }

  # about to potentially modify data
  data <- grid$data

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  if (inherits(data, "nativeRaster")) {
    attrs <- attributes(data)
    dim(data) <- rev(dim(data))
    data <- data[j, i, drop = FALSE]
    attrs$dim <- rev(dim(data))
    attributes(data) <- attrs
  } else {
    # we want to keep everything for existing dimensions
    # this means generating a list of missings to fill
    # the correct number of additional dimensions
    n_more_dims <- length(dim(data)) - 2L
    more_dims <- alist(1, )[rep(2, n_more_dims)]
    data <- do.call("[", c(list(data, i, j), more_dims, list(drop = FALSE)))
  }

  grid$data <- data
  grid$bbox <- indices$bbox
  grid
}

#' @rdname grd_subset
#' @export
grd_subset_indices <- function(grid, i = NULL, j = NULL, bbox = NULL, ...) {
  UseMethod("grd_subset_indices")
}

#' @export
grd_subset_indices.wk_grd_xy <- function(grid, i = NULL, j = NULL, bbox = NULL, ...) {
  grd_subset_indices_internal(grid, i, j, bbox)
}

#' @export
grd_subset_indices.wk_grd_rct <- function(grid, i = NULL, j = NULL, bbox = NULL, ...) {
  grd_subset_indices_internal(grid, i, j, bbox)
}

grd_subset_indices_internal <- function(grid, i = NULL, j = NULL, bbox = NULL) {
  s <- grd_summary(grid)

  # can't get any more subsetted than an empty grid!
  if ((s$nx * s$ny) == 0) {
    return(
      list(
        i = c(start = NA_integer_, stop = NA_integer_, step = NA_integer_),
        j = c(start = NA_integer_, stop = NA_integer_, step = NA_integer_),
        bbox = grid$bbox
      )
    )
  }

  # empty subset criteria -> no subset
  if (is.null(bbox) && is.null(i) && is.null(j)) {
    return(
      list(
        i = c(start = NA_integer_, stop = NA_integer_, step = NA_integer_),
        j = c(start = NA_integer_, stop = NA_integer_, step = NA_integer_),
        bbox = grid$bbox
      )
    )
  }

  if (is.null(bbox) && (!is.null(i) || !is.null(j))) {
    indices <- grd_expand_ij_internal(i, j, s$nx, s$ny)
  } else if (!is.null(bbox) && is.null(i) && is.null(j)) {
    indices <- grd_expand_bbox_rct_internal(grid, bbox, s$dx, s$dy)
  } else {
    stop("Must specify bbox OR (i | j)", call. = FALSE)
  }

  x <- indices$j
  y <- indices$i

  # clamp to actual indices
  x <- x[!is.na(x) & (x >= 1L) & (x <= s$nx)]
  y <- y[!is.na(y) & (y >= 1L) & (y <= s$ny)]

  # sort to avoid flipping any data
  x <- sort(x)
  y <- sort(y)

  # re-define the bbox based on actual index subset
  new_rct <- s[c("xmin", "ymin", "xmax", "ymax")]

  if (inherits(grid, "wk_grd_rct")) {
    # strategy is to keep the cell centres intact on downsample
    if (length(x) > 0) {
      downsample_x <- x[2] - x[1]
      new_dx <- if (length(x) >= 2) s$dx * downsample_x else s$dx
      new_rct$xmin <- s$xmin + ((min(x) - 1) * s$dx) + (s$dx / 2) - (new_dx / 2)
      new_rct$xmax <- s$xmin + (max(x) * s$dx) - (s$dx / 2) + (new_dx / 2)
    } else {
      new_rct$xmin <- Inf
      new_rct$xmax <- -Inf
    }

    if (length(y) > 0) {
      downsample_y <- y[2] - y[1]
      new_dy <- if (length(y) >= 2) s$dy * downsample_y else s$dy
      new_rct$ymin <- s$ymax - (max(y) * s$dy) + (s$dy / 2) - (new_dy / 2)
      new_rct$ymax <- s$ymax - ((min(y) - 1) * s$dy) - (s$dy / 2) + (new_dy / 2)
    } else {
      new_rct$ymin <- Inf
      new_rct$ymax <- -Inf
    }
  } else if (inherits(grid, "wk_grd_xy")) {
    if (length(x) > 0) {
      new_rct$xmin <- s$xmin + (x[1] - 1L) * s$dx
      new_rct$xmax <- s$xmin + (x[length(x)] - 1L) * s$dx
    } else {
      new_rct$xmin <- Inf
      new_rct$xmax <- -Inf
    }

    if (length(y) > 0) {
      new_rct$ymin <- s$ymax - (y[1] - 1L) * s$dy
      new_rct$ymax <- s$ymax - (y[length(y)] - 1L) * s$dy
    } else {
      new_rct$ymin <- Inf
      new_rct$ymax <- -Inf
    }
  } else {
    stop("Unknown grid type", call. = FALSE) # nocov
  }

  # eventually this can be computed more efficiently based on the above
  if (length(y) == 0) {
    i <- c(start = 0L, stop = 0L, step = 1L)
  } else {
    i <- c(start = y[1] - 1L, stop = y[length(y)], step = y[min(2, length(y))] - y[1])
  }

  if (length(x) == 0) {
    j <- c(start = 0L, stop = 0L, step = 1L)
  } else {
    j <- c(start = x[1] - 1L, stop = x[length(x)], step = x[min(2, length(x))] - x[1])
  }

  list(i = i, j = j, bbox = new_wk_rct(new_rct, crs = wk_crs(grid)))
}

grd_expand_bbox_rct_internal <- function(grid, bbox_target, dx, dy) {
  # for access to members
  rct <- unclass(grid$bbox)

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

  if (inherits(grid, "wk_grd_rct")) {
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
  } else if (inherits(grid, "wk_grd_xy")) {
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
