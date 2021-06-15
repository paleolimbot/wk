
#' Plot well-known geometry vectors
#'
#' @inheritParams wkutils::wkt_plot
#'
#' @return The input, invisibly.
#' @importFrom graphics plot
#' @export
#'
#' @examples
#' # requires the wkutils package
#' if (requireNamespace("wkutils")) {
#'   plot(as_wkt("LINESTRING (0 0, 1 1)"))
#'   plot(as_wkb("LINESTRING (0 0, 1 1)"))
#' }
#'
plot.wk_wkt <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "",
                        rule = "evenodd", add = FALSE) {
  wkutils::wkt_plot(
    x,
    ...,
    asp = asp,
    bbox = bbox,
    xlab = xlab,
    ylab = ylab,
    rule = rule,
    add = add
  )

  invisible(x)
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_wkb <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "",
                        rule = "evenodd", add = FALSE) {
  wkutils::wkb_plot(
    x,
    ...,
    asp = asp,
    bbox = bbox,
    xlab = xlab,
    ylab = ylab,
    rule = rule,
    add = add
  )

  invisible(x)
}

wk_plot <- function(x, ..., handler_factory = identity,
                    asp = 1, bbox = NULL, xlab = "", ylab = "",
                    rule = "evenodd", add = FALSE) {
  if (!add) {
    bbox <- unclass(bbox)
    bbox <- bbox %||% unclass(wk_handle(x, handler_factory(wk_bbox_handler())))
    xlim <- c(bbox$xmin, bbox$xmax)
    ylim <- c(bbox$ymin, bbox$ymax)

    graphics::plot(
      numeric(0),
      numeric(0),
      xlim = xlim,
      ylim = ylim,
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }

  if (length(x) == 0) {
    return(invisible(x))
  }

  # in simple cases we can do some fast shortcuts
  meta <- wk_handle(x, handler_factory(wk_meta_handler()))

  # points are handled by as_xy() nicely
  if (all(meta$geometry_type == 1L)) {
    coords <- unclass(wk_handle(x, handler_factory(xy_writer())))
    graphics::points(coords, ...)
    return(invisible(x))
  }

  # evaluate dots
  dots <- list(..., rule = rule)
  is_scalar <- !vapply(dots, vctrs::vec_is, logical(1))
  dots[is_scalar] <- lapply(dots[is_scalar], list)
  dots_lengths <- vapply(dots, length, integer(1))

  # if we have all constant dots (common), we can skip the loop
  constant_dots <- all(dots_lengths == 1L)

  if (constant_dots && all(meta$geometry_type %in% c(1L, 4L))) {
    coords <- unclass(wk_handle(x, handler_factory(wk_vertex_filter(xy_writer()))))
    graphics::points(wk_coords(x)[c("x", "y")], ...)

  } else if (constant_dots && all(meta$geometry_type %in% c(2L, 5L))) {
    coords <- unclass(wk_handle(x, handler_factory(wk_vertex_filter(xy_writer(), add_details = TRUE))))
    geom_id <- attr(coords, "wk_details")$part_id
    geom_id_lag <- c(-1L, geom_id[-length(geom_id)])
    new_geom <- geom_id != geom_id_lag
    na_shift <- cumsum(new_geom) - 1L
    coords_seq <- seq_along(geom_id)

    coord_x <- rep(NA_real_, length(geom_id) + sum(new_geom) - 1L)
    coord_y <- rep(NA_real_, length(geom_id) + sum(new_geom) - 1L)

    coord_x[coords_seq + na_shift] <- coords$x
    coord_y[coords_seq + na_shift] <- coords$y

    graphics::lines(coord_x, coord_y, ...)

  } else if (constant_dots && all(meta$geometry_type %in% c(3L, 6L))) {
    coords <- unclass(wk_handle(x, handler_factory(wk_vertex_filter(xy_writer(), add_details = TRUE))))
    geom_id <- attr(coords, "wk_details")$ring_id
    geom_id_lag <- c(-1L, geom_id[-length(geom_id)])
    new_geom <- geom_id != geom_id_lag
    na_shift <- cumsum(new_geom) - 1L
    coords_seq <- seq_along(geom_id)

    coord_x <- rep(NA_real_, length(geom_id) + sum(new_geom) - 1L)
    coord_y <- rep(NA_real_, length(geom_id) + sum(new_geom) - 1L)

    coord_x[coords_seq + na_shift] <- coords$x
    coord_y[coords_seq + na_shift] <- coords$y

    graphics::polypath(coord_x, coord_y, ..., rule = rule)
  } else {
    stop("Collections and mixed types plotting is not implemented")
  }

  invisible(x)
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_xy <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "", add = FALSE) {
  x_bare <- unclass(x)

  if (!add) {
    graphics::plot(
      double(), double(),
      xlim = range(x_bare$x, finite = TRUE),
      ylim = range(x_bare$y, finite = TRUE),
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }

  graphics::points(x_bare$x, x_bare$y, ...)

  invisible(x)
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_rct <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "", add = FALSE) {
  x_bare <- unclass(x)

  if (!add) {
    xlim_min <- range(x_bare$xmin, finite = TRUE)
    xlim_max <- range(x_bare$xmax, finite = TRUE)
    ylim_min <- range(x_bare$ymin, finite = TRUE)
    ylim_max <- range(x_bare$ymax, finite = TRUE)

    graphics::plot(
      double(), double(),
      xlim = range(c(xlim_min, xlim_max), finite = TRUE),
      ylim = range(c(ylim_min, ylim_max), finite = TRUE),
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }

  graphics::rect(x_bare$xmin, x_bare$ymin, x_bare$xmax, x_bare$ymax, ...)
  invisible(x)
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_crc <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "",
                        add = FALSE) {
  x_bare <- unclass(x)

  if (!add) {
    xlim_min <- range(x_bare$x + x_bare$r, finite = TRUE)
    xlim_max <- range(x_bare$x - x_bare$r, finite = TRUE)
    ylim_min <- range(x_bare$y + x_bare$r, finite = TRUE)
    ylim_max <- range(x_bare$y - x_bare$r, finite = TRUE)

    graphics::plot(
      double(), double(),
      xlim = range(c(xlim_min, xlim_max), finite = TRUE),
      ylim = range(c(ylim_min, ylim_max), finite = TRUE),
      xlab = xlab,
      ylab = ylab,
      asp = asp
    )
  }

  # estimate resolution for turning circles into segments
  usr <- graphics::par("usr")
  usr_x <- usr[1:2]
  usr_y <- usr[3:4]
  device_x <- graphics::grconvertX(usr_x, to = "device")
  device_y <- graphics::grconvertY(usr_y, to = "device")

  # Use resolution of 1 at the device level, scale to usr coords.
  # Changing this number to 2 or 4 doesn't really affect the speed
  # at which these plot; a value of 1 tends to give very good
  # resolution and is acceptable even when a plot in the interactive
  # device is zoomed.
  scale_x <- diff(device_x) / diff(usr_x)
  scale_y <- diff(device_y) / diff(usr_y)
  scale <- min(abs(scale_x), abs(scale_y))
  resolution_usr <- 1 / scale

  plot(
    wk_handle(x, wkb_writer(), resolution = resolution_usr),
    ...,
    add = TRUE
  )

  invisible(x)
}
