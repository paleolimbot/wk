
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

  # use resolution of 1 at the device level, scale to usr coords
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
