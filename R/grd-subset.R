
#' Subset grid objects
#'
#' The [grd_subset()] method handles the subsetting of a [grd()]
#' in x-y space. Ordering of indices is not considered and logical
#' indies are recycled silently along dimensions. The result of
#' a [grd_subset()] is always a [grd()] of the same type whose
#' relationship to x-y space has not changed.
#'
#' @inheritParams grd_cell
#' @inheritParams grd_summary
#' @param grid_data The `data` member of a [grd()]. This is typically an
#'   array but can also be an S3 object with an array-like subset method.
#'   The [native raster][grDevices::as.raster] is special-cased as its
#'   subset method requires non-standard handling.
#' @param i,j 1-based index values. `i` indices correspond to decreasing
#'   `y` values; `j` indices correspond to increasing `x` values.
#'   Values outside the range `1:nrow|ncol(data)` will be censored to
#'   `NA` including 0 and negative values.
#' @param ... Passed to subset methods
#'
#' @return A modified `grid` whose cell centres have not changed location
#'   as a result of the subset.
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_subset(grid, 1:20, 1:30)
#' grd_crop(grid, rct(-10, -10, 10, 10))
#' grd_extend(grid, rct(-10, -10, 10, 10))
#'
grd_subset <- function(grid, i = NULL, j = NULL, ...) {
  UseMethod("grd_subset")
}

#' @rdname grd_subset
#' @export
grd_crop <- function(grid, bbox, ..., step = 1L, snap = NULL) {
  UseMethod("grd_crop")
}

#' @rdname grd_subset
#' @export
grd_extend <- function(grid, bbox, ..., step = 1L, snap = NULL) {
  UseMethod("grd_extend")
}

#' @export
grd_subset.grd_rct <- function(grid, i = NULL, j = NULL, ...) {
  grd_subset_grd_internal(grid, i, j)
}

#' @export
grd_subset.grd_xy <- function(grid, i = NULL, j = NULL, ...) {
  grd_subset_grd_internal(grid, i, j)
}

grd_subset_grd_internal <- function(grid, i = NULL, j = NULL) {
  ij <- ij_from_args(i, j)

  # convert i and j into start, stop, step
  i <- ij_to_slice_one(ij$i, dim(grid)[1])
  j <- ij_to_slice_one(ij$j, dim(grid)[2])

  # calculate bbox
  s <- grd_summary(grid)
  dx <- unname(j["step"] * s$dx)
  dy <- unname(i["step"] * s$dy)
  center_min <- unclass(grd_cell_xy(grid, i["stop"], j["start"] + 1L))
  center_max <- unclass(grd_cell_xy(grid, i["start"] + 1L, j["stop"]))
  rct_new <- list(
    xmin = center_min$x, ymin = center_min$y,
    xmax = center_max$x, ymax = center_max$y
  )

  # check for empty subsets in both directions
  if (!is.finite(rct_new$xmax - rct_new$xmin)) {
    rct_new$xmin <- Inf
    rct_new$xmax <- -Inf
  } else if (inherits(grid, "grd_rct")) {
    rct_new$xmin <- rct_new$xmin - dx / 2
    rct_new$xmax <- rct_new$xmax + dx / 2
  }

  if (!is.finite(rct_new$ymax - rct_new$ymin)) {
    rct_new$ymin <- Inf
    rct_new$ymax <- -Inf
  } else if (inherits(grid, "grd_rct")) {
    rct_new$ymin <- rct_new$ymin - dy / 2
    rct_new$ymax <- rct_new$ymax + dy / 2
  }

  grid$data <- grd_data_subset(grid$data, i, j)
  grid$bbox <- new_wk_rct(rct_new, crs = wk_crs(grid))
  grid
}

#' @rdname grd_subset
#' @export
grd_crop.grd_rct <- function(grid, bbox, ..., step = 1L, snap = NULL) {
  snap <- snap %||% list(grd_snap_next, grd_snap_previous)
  ij <- grd_cell_range(grid, bbox, step = step, snap = snap)

  ij$i["start"] <- max(ij$i["start"], 0L)
  ij$i["stop"] <- min(ij$i["stop"], dim(grid)[1])
  ij$j["start"] <- max(ij$j["start"], 0L)
  ij$j["stop"] <- min(ij$j["stop"], dim(grid)[2])

  grd_subset(grid, ij)
}

#' @rdname grd_subset
#' @export
grd_crop.grd_xy <- function(grid, bbox, ..., step = 1L, snap = NULL) {
  snap <- snap %||% list(ceiling, floor)
  ij <- grd_cell_range(grid, bbox, step = step, snap = snap)

  ij$i["start"] <- max(ij$i["start"], 0L)
  ij$i["stop"] <- min(ij$i["stop"], dim(grid)[1])
  ij$j["start"] <- max(ij$j["start"], 0L)
  ij$j["stop"] <- min(ij$j["stop"], dim(grid)[2])

  grd_subset(grid, ij)
}

#' @rdname grd_subset
#' @export
grd_extend.grd_rct <- function(grid, bbox, ..., step = 1L, snap = NULL) {
  snap <- snap %||% list(grd_snap_next, grd_snap_previous)
  grd_subset(grid, grd_cell_range(grid, bbox, step = step, snap = snap))
}

#' @rdname grd_subset
#' @export
grd_extend.grd_xy <- function(grid, bbox, ...,  step = 1L, snap = NULL) {
  snap <- snap %||% list(ceiling, floor)
  grd_subset(grid, grd_cell_range(grid, bbox, step = step, snap = snap))
}

#' @rdname grd_subset
#' @export
grd_data_subset <- function(grid_data, i = NULL, j = NULL) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  if (inherits(grid_data, "nativeRaster")) {
    # special case the nativeRaster, whose dims are lying about
    # the ordering needed to index it
    attrs <- attributes(grid_data)
    dim(grid_data) <- rev(dim(grid_data))
    grid_data <- grid_data[ij$j, ij$i, drop = FALSE]
    attrs$dim <- rev(dim(grid_data))
    attributes(grid_data) <- attrs
    grid_data
  } else {
    # we want to keep everything for existing dimensions
    # this means generating a list of missings to fill
    # the correct number of additional dimensions
    n_more_dims <- length(dim(grid_data)) - 2L
    more_dims <- alist(1, )[rep(2, n_more_dims)]
    do.call("[", c(list(grid_data, ij$i, ij$j), more_dims, list(drop = FALSE)))
  }
}
