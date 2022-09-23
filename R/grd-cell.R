
#' Grid cell operators
#'
#' @inheritParams grd_summary
#' @inheritParams grd_data
#' @param bbox An [rct()] object.
#' @param out_of_bounds One of 'keep', 'censor', 'discard', or 'squish'
#' @param step The difference between adjascent indices in the output
#' @param point A [handleable][wk_handle] of points.
#' @param snap A function that transforms real-valued indices to integer
#'   indices (e.g., [floor()], [ceiling()], or [round()]).
#'   For [grd_cell_range()], a `list()` with exactly two elements to be called
#'   for the minimum and maximum index values, respectively.
#' @param ... Unused
#'
#' @return
#'   - `grd_cell()`: returns a `list(i, j)` of index values corresponding
#'     to the input points and adjusted according to `snap`. Index values
#'     will be outside `dim(grid)` for points outside `wk_bbox(grid)` including
#'     negative values.
#'   - `grd_cell_range()` returns a slice describing the range of indices
#'     in the `i` and `j` directions.
#'   - `grd_cell_rct()` returns a [rct()] of the cell extent at `i, j`.
#'   - `grd_cell_xy()` returns a [xy()] of the cell center at `i, j`.
#' @export
#'
#' @examples
#' grid <- grd(nx = 3, ny = 2)
#' grd_cell(grid, xy(0.5, 0.5))
#' grd_cell_range(grid, grid$bbox)
#' grd_cell_rct(grid, 1, 1)
#' grd_cell_xy(grid, 1, 1)
#'
grd_cell <- function(grid, point, ..., snap = grd_snap_next) {
  UseMethod("grd_cell")
}

#' @export
grd_cell.grd_rct <- function(grid, point, ..., snap = grd_snap_next) {
  s <- grd_summary(grid)
  point <- unclass(as_xy(point))
  i <- if (s$width == -Inf) rep(NA_real_, length(point$x)) else (s$ymax - point$y) / s$dy
  j <- if (s$height == -Inf) rep(NA_real_, length(point$x)) else (point$x - s$xmin) / s$dx
  new_data_frame(list(i = unname(snap(i - 0.5) + 1L), j = unname(snap(j - 0.5) + 1L)))
}

#' @export
grd_cell.grd_xy <- function(grid, point, ..., snap = grd_snap_next) {
  grd_cell(as_grd_rct(grid), point, snap = snap)
}

#' @rdname grd_cell
#' @export
grd_cell_range <- function(grid, bbox = wk_bbox(grid), ..., step = 1L, snap = grd_snap_next) {
  UseMethod("grd_cell_range")
}

#' @export
grd_cell_range.default <- function(grid, bbox = wk_bbox(grid), ..., step = 1L, snap = grd_snap_next) {
  # normalized so that xmin < xmax, ymin < ymax
  if (inherits(bbox, "wk_rct")) {
    bbox <- wk_bbox(as_wkb(bbox))
  } else {
    bbox <- wk_bbox(bbox)
  }

  # step can be length to for i, j steps
  if (length(step) == 1L) {
    step <- step[c(1L, 1L)]
  }

  if (is.function(snap)) {
    snap <- list(snap, snap)
  }

  indices <- grd_cell(grid, as_xy(wk_vertices(bbox)))

  # return a consistent value for an empty grid subset
  s <- grd_summary(grid)
  rct_target <- unclass(bbox)
  rct_target_width <- rct_target$xmax - rct_target$xmin
  rct_target_height <- rct_target$ymax - rct_target$ymin

  indices_min <- grd_cell(grid, xy(rct_target$xmin, rct_target$ymax), snap = snap[[1]])
  indices_max <- grd_cell(grid, xy(rct_target$xmax, rct_target$ymin), snap = snap[[2]])

  if (rct_target_height == -Inf || s$height == -Inf) {
    i <- integer()
  } else {
    i <- c(start = indices_min$i - 1L, stop = indices_max$i, step = 1L)
  }

  if (rct_target_width == -Inf || s$width == -Inf) {
    j <- integer()
  } else {
    j <- c(start = indices_min$j - 1L, stop = indices_max$j, step = 1L)
  }

  # process downsample if requested
  if (!identical(step, c(1L, 1L))) {
    n <- c(0L, 0L)
    if (!identical(i, integer())) {
      n[1] <- i["stop"] - i["start"]
    }

    if (!identical(j, integer())) {
      n[2] <- j["stop"] - j["start"]
    }

    step <- pmin(n, pmax(1L, step))

    if (!identical(i, integer())) {
      i["step"] <- step[1]
      i["start"] <- i["start"] + (step[1] %/% 2L)
      i["stop"] <- i["stop"] - ((step[1] + 1L) %/% 2L)
    }

    if (!identical(j, integer())) {
      j["step"] <- step[2]
      j["start"] <- j["start"] + (step[2] %/% 2L)
      j["stop"] <- j["stop"] - ((step[2] + 1L) %/% 2L)
    }
  }

  list(i = i, j = j)
}

#' @rdname grd_cell
#' @export
grd_cell_rct <- function(grid, i, j = NULL, ...) {
  UseMethod("grd_cell_rct")
}

#' @rdname grd_cell
#' @export
grd_cell_rct.grd_rct <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  s <- grd_summary(grid)

  # non-numeric values don't make sense here because i and j are vectorized
  # instead of crossed to form the final values
  ij <- ij_from_args(i, j)
  if (!is.numeric(ij$i) || !is.numeric(ij$j)) {
    stop("`i` and `j` must be numeric index vectors in grd_cell_rct()")
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

#' @rdname grd_cell
#' @export
grd_cell_rct.grd_xy <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  grd_cell_rct(as_grd_rct(grid), i, j, out_of_bounds = out_of_bounds)
}

#' @rdname grd_cell
#' @export
grd_cell_xy <- function(grid, i, j = NULL, ...) {
  UseMethod("grd_cell_xy")
}

#' @rdname grd_cell
#' @export
grd_cell_xy.grd_rct <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  s <- grd_summary(grid)

  # non-numeric values don't make sense here because i and j are vectorized
  # instead of crossed to form the final values
  ij <- ij_from_args(i, j)
  if (!is.numeric(ij$i) || !is.numeric(ij$j)) {
    stop("`i` and `j` must be numeric index vectors in grd_cell_rct()")
  }

  # recycle to a common length
  ij[] <- recycle_common(ij$i, ij$j)

  # handle out_of_bounds
  ij <- ij_handle_out_of_bounds2(ij, list(s$ny, s$nx), out_of_bounds)

  x <- s$xmin + (ij$j - 1) * s$dx + s$dx / 2
  y <- s$ymax - (ij$i - 1) * s$dy - s$dy / 2

  xy(x, y, crs = wk_crs(grid))
}

#' @rdname grd_cell
#' @export
grd_cell_xy.grd_xy <- function(grid, i, j = NULL, ..., out_of_bounds = "keep") {
  grd_cell_xy(as_grd_rct(grid), i, j, out_of_bounds = out_of_bounds)
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
