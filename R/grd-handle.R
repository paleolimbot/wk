
#' Handler interface for grid objects
#'
#' @inheritParams wk_handle
#' @param data_order A vector of length 2 describing the order in which
#'   values should appear. The default, `c("y", "x")`, will output values
#'   in the same order as the default matrix storage in R (column-major).
#'   You can prefix a dimension with `-` to reverse the order of a
#'   dimension (e.g., `c("-y", "x")`).
#'
#' @return The result of the `handler`.
#' @export
#'
#' @examples
#' wk_handle(grd(nx = 3, ny = 3), wkt_writer())
#' wk_handle(grd(nx = 3, ny = 3, type = "centers"), wkt_writer())
#'
wk_handle.wk_grd_xy <- function(handleable, handler, ..., data_order = c("y", "x")) {
  # eventually these will be more efficient and not resolve every cell
  wk_handle(as_xy(handleable, data_order = data_order), handler, ...)
}

#' @rdname wk_handle.wk_grd_xy
#' @export
wk_handle.wk_grd_rct <- function(handleable, handler, ..., data_order = c("y", "x")) {
  # eventually these will be more efficient and not resolve every cell
  wk_handle(as_rct(handleable, data_order = data_order), handler, ...)
}

#' @export
as_xy.wk_grd_xy <- function(x, ..., data_order = c("y", "x")) {
  rct <- unclass(x$bbox)
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin

  if (identical(width, -Inf) || identical(height, -Inf)) {
    return(xy(crs = wk_crs(x)))
  }

  if (nx == 1L) {
    xs <- rct$xmin
  } else {
    xs <- seq(rct$xmin, rct$xmax, by = width / (nx - 1))
  }

  if (ny == 1L) {
    ys <- rct$ymin
  } else {
    ys <- seq(rct$ymax, rct$ymin, by = -height / (ny - 1))
  }

  # Custom ordering such that coordinates can match up to data
  dim_order <- gsub("^[+-]", "", data_order)

  if (identical(dim_order, c("y", "x"))) {
    if (startsWith(data_order[1], "-")) {
      ys <- rev(ys)
    }

    if (startsWith(data_order[2], "-")) {
      xs <- rev(xs)
    }

    xy(
      rep(xs, each = length(ys)),
      rep(ys, length(xs)),
      crs = wk_crs(x$bbox)
    )
  } else {
    if (startsWith(data_order[2], "-")) {
      ys <- rev(ys)
    }

    if (startsWith(data_order[1], "-")) {
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
as_rct.wk_grd_rct <- function(x, ..., data_order = c("y", "x")) {
  rct <- unclass(x$bbox)
  nx <- dim(x$data)[2]
  ny <- dim(x$data)[1]
  width <- rct$xmax - rct$xmin
  height <- rct$ymax - rct$ymin

  if (identical(width, -Inf) || identical(height, -Inf)) {
    return(rct(crs = wk_crs(x)))
  }

  # Custom ordering such that coordinates can match up to data
  xs <- seq(rct$xmin, rct$xmax, by = width / nx)
  ys <- seq(rct$ymax, rct$ymin, by = -height / ny)

  dim_order <- gsub("^[+-]", "", data_order)

  if (identical(dim_order, c("y", "x"))) {
    if (startsWith(data_order[1], "-")) {
      ys <- rev(ys)
      ymax <- rep(ys[-1], nx)
      ymin <- rep(ys[-length(ys)], nx)
    } else {
      ymin <- rep(ys[-1], nx)
      ymax <- rep(ys[-length(ys)], nx)
    }

    if (startsWith(data_order[2], "-")) {
      xs <- rev(xs)
      xmax <- rep(xs[-length(xs)], each = ny)
      xmin <- rep(xs[-1], each = ny)
    } else {
      xmin <- rep(xs[-length(xs)], each = ny)
      xmax <- rep(xs[-1], each = ny)
    }

    rct(xmin, ymin, xmax, ymax, crs = wk_crs(x$bbox))
  } else {
    if (startsWith(data_order[2], "-")) {
      ys <- rev(ys)
      ymax <- rep(ys[-1], each = nx)
      ymin <- rep(ys[-length(ys)], each = nx)
    } else {
      ymin <- rep(ys[-1], each = nx)
      ymax <- rep(ys[-length(ys)], each = nx)
    }

    if (startsWith(data_order[1], "-")) {
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
