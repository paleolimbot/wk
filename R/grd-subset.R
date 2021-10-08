
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
#' @param out_of_bounds One of 'keep', 'censor', 'discard', or 'squish'
#' @param point A [handleable][wk_handle] of points.
#' @param snap A function that transforms real-valued indices to integer
#'   indices (e.g., [floor()], [ceiling()], or [round()]).
#'   For [grd_index_range()], a `list()` with exactly two elements to be called
#'   for the minimum and maximum index values, respectively.
#' @param ... Passed to subset methods
#'
#' @return
#'   - `grd_index()`: returns a `list(i, j)` of index values corresponding
#'     to the input points and adjusted according to `snap`. Index values
#'     will be outside `dim(grid)` for points outside `wk_bbox(grid)` including
#'     negative values.
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
grd_subset <- function(grid, i = NULL, j = NULL, ...) {
  UseMethod("grd_subset")
}

#' @export
grd_subset.wk_grd_rct <- function(grid, i = NULL, j = NULL, ...) {
  grd_subset_grd_internal(grid, i, j)
}

#' @export
grd_subset.wk_grd_xy <- function(grid, i = NULL, j = NULL, ...) {
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
  center_min <- unclass(grd_cell_center(grid, i["stop"], j["start"] + 1L))
  center_max <- unclass(grd_cell_center(grid, i["start"] + 1L, j["stop"]))
  rct_new <- list(
    xmin = center_min$x, ymin = center_min$y,
    xmax = center_max$x, ymax = center_max$y
  )

  # check for empty subsets in both directions
  if (!is.finite(rct_new$xmax - rct_new$xmin)) {
    rct_new$xmin <- Inf
    rct_new$xmax <- -Inf
  } else if (inherits(grid, "wk_grd_rct")) {
    rct_new$xmin <- rct_new$xmin - dx / 2
    rct_new$xmax <- rct_new$xmax + dx / 2
  }

  if (!is.finite(rct_new$ymax - rct_new$ymin)) {
    rct_new$ymin <- Inf
    rct_new$ymax <- -Inf
  } else if (inherits(grid, "wk_grd_rct")) {
    rct_new$ymin <- rct_new$ymin - dy / 2
    rct_new$ymax <- rct_new$ymax + dy / 2
  }

  grid$data <- grd_subset_data(grid$data, i, j)
  grid$bbox <- new_wk_rct(rct_new, crs = wk_crs(grid))
  grid
}

#' @rdname grd_subset
#' @export
grd_subset_data <- function(grid_data, i = NULL, j = NULL, ...) {
  UseMethod("grd_subset_data")
}

#' @export
grd_subset_data.default <- function(grid_data, i = NULL, j = NULL, ...) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  if (inherits(grid_data, "nativeRaster")) {
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

#' @rdname grd_subset
#' @export
grd_crop <- function(grid, bbox, ..., snap = list(ceiling, floor)) {
  UseMethod("grd_crop")
}

#' @rdname grd_subset
#' @export
grd_crop.wk_grd_rct <- function(grid, bbox, ..., snap = grd_snap_previous) {
  ij <- grd_index_range(grid, bbox, snap = snap)

  ij$i["start"] <- max(ij$i["start"], 0L)
  ij$i["stop"] <- min(ij$i["stop"], dim(grid)[1])
  ij$j["start"] <- max(ij$j["start"], 0L)
  ij$j["stop"] <- min(ij$j["stop"], dim(grid)[2])

  grd_subset(grid, ij)
}

#' @rdname grd_subset
#' @export
grd_crop.wk_grd_xy <- function(grid, bbox, ..., snap = list(ceiling, floor)) {
  ij <- grd_index_range(grid, bbox, snap = snap)

  ij$i["start"] <- max(ij$i["start"], 0L)
  ij$i["stop"] <- min(ij$i["stop"], dim(grid)[1])
  ij$j["start"] <- max(ij$j["start"], 0L)
  ij$j["stop"] <- min(ij$j["stop"], dim(grid)[2])

  grd_subset(grid, ij)
}

#' @rdname grd_subset
#' @export
grd_extend <- function(grid, bbox, ..., snap = list(ceiling, floor)) {
  UseMethod("grd_extend")
}

#' @rdname grd_subset
#' @export
grd_extend.wk_grd_rct <- function(grid, bbox, ..., snap = list(grd_snap_next, grd_snap_previous)) {
  grd_subset(grid, grd_index_range(grid, bbox, snap = snap))
}

#' @rdname grd_subset
#' @export
grd_extend.wk_grd_xy <- function(grid, bbox, ..., snap = list(ceiling, floor)) {
  grd_subset(grid, grd_index_range(grid, bbox, snap = snap))
}

#' @rdname grd_subset
#' @export
grd_index <- function(grid, point, ..., snap = grd_snap_next) {
  UseMethod("grd_index")
}

#' @export
grd_index.wk_grd_rct <- function(grid, point, ..., snap = grd_snap_next) {
  s <- grd_summary(grid)
  point <- unclass(as_xy(point))
  i <- if (s$width == -Inf) rep(NA_real_, length(point$x)) else (s$ymax - point$y) / s$dy
  j <- if (s$height == -Inf) rep(NA_real_, length(point$x)) else (point$x - s$xmin) / s$dx
  list(i = snap(i - 0.5) + 1L, j = snap(j - 0.5) + 1L)
}

#' @export
grd_index.wk_grd_xy <- function(grid, point, ..., snap = grd_snap_next) {
  grd_index(as_grd_rct(grid), point, snap = snap)
}

#' @rdname grd_subset
#' @export
grd_index_range <- function(grid, bbox, ..., snap = grd_snap_next) {
  UseMethod("grd_index_range")
}

#' @export
grd_index_range.default <- function(grid, bbox, ..., snap = grd_snap_next) {
  # normalized so that xmin < xmax, ymin < ymax
  if (inherits(bbox, "wk_rct")) {
    bbox <- wk_bbox(as_wkb(bbox))
  } else {
    bbox <- wk_bbox(bbox)
  }

  if (is.function(snap)) {
    snap <- list(snap, snap)
  }

  indices <- grd_index(grid, as_xy(wk_vertices(bbox)))

  # return a consistent value for an empty grid subset
  s <- grd_summary(grid)
  rct_target <- unclass(bbox)
  rct_target_width <- rct_target$xmax - rct_target$xmin
  rct_target_height <- rct_target$ymax - rct_target$ymin

  indices_min <- grd_index(grid, xy(rct_target$xmin, rct_target$ymax), snap = snap[[1]])
  indices_max <- grd_index(grid, xy(rct_target$xmax, rct_target$ymin), snap = snap[[2]])

  if (rct_target_height == -Inf || s$height == -Inf) {
    i <- integer()
  } else {
    i <- c(start = indices_min$i - 1L, stop = indices_max$i, step = NA_integer_)
  }

  if (rct_target_width == -Inf || s$width == -Inf) {
    j <- integer()
  } else {
    j <- c(start = indices_min$j - 1L, stop = indices_max$j, step = NA_integer_)
  }

  list(i = i, j = j)
}

#' @rdname grd_subset
#' @export
grd_cell_bounds <- function(grid, i, j = NULL, ...) {
  UseMethod("grd_cell_bounds")
}

#' @rdname grd_subset
#' @export
grd_cell_bounds.wk_grd_rct <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  s <- grd_summary(grid)

  # non-numeric values don't make sense here because i and j are vectorized
  # instead of crossed to form the final values
  ij <- ij_from_args(i, j)
  if (!is.numeric(ij$i) || !is.numeric(ij$j)) {
    stop("`i` and `j` must be numeric index vectors in grd_cell_bounds()")
  }

  # recycle to a common length
  ij[] <- recycle_common(ij$i, ij$j)

  # handle out_of_bounds
  ij <- ij_handle_out_of_bounds2(ij, list(s$ny, s$nx), out_of_bounds)

  xmin <- s$xmin + (ij$j - 1) * s$dx
  xmax <- s$xmin + ij$j * s$dx
  ymin <- s$ymax - ij$i * s$dy
  ymax <- s$ymax - (ij$i - 1) * s$dy

  rct(xmin, ymin, xmax, ymax, crs = wk_crs(grid))
}

#' @rdname grd_subset
#' @export
grd_cell_bounds.wk_grd_xy <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  grd_cell_bounds(as_grd_rct(grid), i, j, out_of_bounds = out_of_bounds)
}

#' @rdname grd_subset
#' @export
grd_cell_center <- function(grid, i, j = NULL, ...) {
  UseMethod("grd_cell_center")
}

#' @rdname grd_subset
#' @export
grd_cell_center.wk_grd_rct <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  s <- grd_summary(grid)

  # non-numeric values don't make sense here because i and j are vectorized
  # instead of crossed to form the final values
  ij <- ij_from_args(i, j)
  if (!is.numeric(ij$i) || !is.numeric(ij$j)) {
    stop("`i` and `j` must be numeric index vectors in grd_cell_bounds()")
  }

  # recycle to a common length
  ij[] <- recycle_common(ij$i, ij$j)

  # handle out_of_bounds
  ij <- ij_handle_out_of_bounds2(ij, list(s$ny, s$nx), out_of_bounds)

  x <- s$xmin + (ij$j - 1) * s$dx + s$dx / 2
  y <- s$ymax - (ij$i - 1) * s$dy - s$dy / 2

  xy(x, y, crs = wk_crs(grid))
}

#' @rdname grd_subset
#' @export
grd_cell_center.wk_grd_xy <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  grd_cell_center(as_grd_rct(grid), i, j, out_of_bounds = out_of_bounds)
}

ij_from_args <- function(i, j = NULL) {
  if (is.null(i) && is.null(j)) {
    list(i = NULL, j = NULL)
  } else if (is.null(j) && is.list(i)) {
    i
  } else {
    list(i = i, j = j)
  }
}

ij_expand_one <- function(i, n, out_of_bounds = "keep") {
  if (is.null(i)) {
    i <- if (n > 0) seq(1L, n) else integer()
  } else if (identical(names(i), c("start", "stop", "step"))) {
    value_na <- is.na(i)
    i[value_na] <- c(0L, n, 1L)[value_na]
    if (i["stop"] > i["start"]) {
      i <- unname(seq(i["start"] + 1L, i["stop"], by = i["step"]))
    } else {
      i <- integer()
    }
  } else if (is.numeric(i)) {
    i <- i
  } else {
    stop(
      "index vectors must be NULL, numeric, or c(start = , stop =, step =)",
      call. = FALSE
    )
  }

  if (out_of_bounds == "censor") {
    i[(i > n) | (i < 1)] <- NA_integer_
  } else if (out_of_bounds == "keep") {
    # do nothing
  } else if (out_of_bounds == "discard") {
    i <- i[(i <= n) & (i >= 1)]
  } else if (out_of_bounds == "squish") {
    i[i < 1L] <- 1L
    i[i > n] <- n
  } else {
    stop(
      "`out_of_bounds` must be one of 'censor', 'keep', 'discard', or 'squish'",
      call. = FALSE
    )
  }

  i
}

ij_to_slice_one <- function(i, n) {
  if (is.null(i)) {
    i <- if (n == 0L) integer() else c(start = 0L, stop = n, step = 1L)
  } else if (identical(names(i), c("start", "stop", "step"))) {
    value_na <- is.na(i)
    i[value_na] <- c(0L, n, 1L)[value_na]
    i <- if (i["start"] >= i["stop"]) integer() else i
  } else if (is.numeric(i)) {
    if (length(i) == 0L) {
      i <- integer()
    } else if ((length(i) == 1L) && is.finite(i)) {
      i <- c(start = i - 1L, stop = i, step = 1L)
    } else {
      if (any(!is.finite(i))) {
        stop("numeric index vectors must be finite in `grd_subset()`", call. = FALSE)
      }

      step <- unique(diff(i))
      if ((length(step) != 1) || (step <= 0)) {
        stop("numeric index vectors must be equally spaced and ascending", call. = FALSE)
      }

      i <- c(start = min(i) - 1L, stop = max(i), step = step)
    }
  } else {
    stop(
      "index vectors must be NULL, numeric, or c(start = , stop =, step =)",
      call. = FALSE
    )
  }

  i
}

# used by extractors to handle out-of-bounds points and/or cells
ij_handle_out_of_bounds2 <- function(ij, n, out_of_bounds) {
  if (out_of_bounds == "keep") {
    return(ij)
  }

  oob_i <- !is.na(ij$i) & ((ij$i > n[[1]]) | (ij$i < 1L))
  oob_j <- !is.na(ij$j) & ((ij$j > n[[2]]) | (ij$j < 1L))
  oob_either <- oob_i | oob_j
  if (!any(oob_either)) {
    return(ij)
  }

  if (out_of_bounds == "censor") {
    ij$i[oob_either] <- NA_integer_
    ij$j[oob_either] <- NA_integer_
  } else if (out_of_bounds == "discard") {
    ij$i <- ij$i[!oob_either]
    ij$j <- ij$j[!oob_either]
  } else if (out_of_bounds == "squish") {
    ij$i[!is.na(ij$i) & (ij$i < 1L)] <- 1L
    ij$j[!is.na(ij$j) & (ij$j < 1L)] <- 1L
    ij$i[!is.na(ij$i) & (ij$i > n[[1]])] <- n[[1]]
    ij$j[!is.na(ij$j) & (ij$j > n[[2]])] <- n[[2]]
  } else {
    stop(
      "`out_of_bounds` must be one of 'censor', 'keep', 'discard', or 'squish'",
      call. = FALSE
    )
  }

  ij
}

#' Index snap functions
#'
#' These functions can be used in [grd_index()] and
#' [grd_index_range()]. These functions differ in the way
#' they round 0.5: [grd_snap_next()] always rounds up
#' and [grd_snap_previous()] always rounds down. You can
#' also use [floor()] and [ceiling()] as index
#' snap functions.
#'
#' @param x A vector of rescaled but non-integer indices
#'
#' @return A vector of integer indices
#' @export
#'
#' @examples
#' grd_snap_next(seq(0, 2, 0.25))
#' grd_snap_previous(seq(0, 2, 0.25))
#'
grd_snap_next <- function(x) {
  ifelse(((x + 0.5) %% 1L) == 0, ceiling(x), round(x))
}

#' @rdname grd_snap_next
#' @export
grd_snap_previous <- function(x) {
  ifelse(((x + 0.5) %% 1L) == 0, floor(x), round(x))
}
