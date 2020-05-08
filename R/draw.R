
#' Draw well-known geometries
#'
#' These functions send well-known geometry vectors to a
#' graphics device using [graphics::points()],
#' [graphics::lines()], and [graphics::polypath()]. These are
#' minimal wrappers aimed at developers who need to visualize
#' test data: they do not check geometry type and are unlikely
#' to work with vectorized graphical parameters in `...`. Use
#' the `wk*_plot_new()` functions to initialize a plot using the
#' extent of all coordinates in the vector.
#'
#' @inheritParams wkb_translate_wkt
#' @param ... Passed to [graphics::points()],
#'   [graphics::lines()], or [graphics::polypath()]
#' @param rule Passed to [graphics::polypath()]
#' @param xlab,ylab,main Passed to [graphics::plot()] to
#'   initialize a new plot.
#'
#' @return The input, invisibly
#' @export
#'
#' @examples
#' x <- "POLYGON ((0 0, 10 0, 10 10, 0 10, 0 0))"
#'
#' wkt_plot_new(x)
#' wkt_draw_polypath(x, col = "grey90")
#' wkt_draw_lines(x, col = "red")
#' wkt_draw_points(x)
#'
wkb_draw_points <- function(wkb, ...) {
  wkcoords_draw_points(wkb_coords(wkb), ...)
  invisible(wkb)
}

#' @rdname wkb_draw_points
#' @export
wkt_draw_points <- function(wkt, ...) {
  wkcoords_draw_points(wkt_coords(wkt), ...)
  invisible(wkt)
}

#' @rdname wkb_draw_points
#' @export
wksxp_draw_points <- function(wksxp, ...) {
  wkcoords_draw_points(wksxp_coords(wksxp), ...)
  invisible(wksxp)
}

#' @rdname wkb_draw_points
#' @export
wkb_draw_lines <- function(wkb, ...) {
  wkcoords_draw_lines(wkb_coords(wkb, sep_na = TRUE), ...)
  invisible(wkb)
}

#' @rdname wkb_draw_points
#' @export
wkt_draw_lines <- function(wkt, ...) {
  wkcoords_draw_lines(wkt_coords(wkt, sep_na = TRUE), ...)
  invisible(wkt)
}

#' @rdname wkb_draw_points
#' @export
wksxp_draw_lines <- function(wksxp, ...) {
  wkcoords_draw_lines(wksxp_coords(wksxp, sep_na = TRUE), ...)
  invisible(wksxp)
}

#' @rdname wkb_draw_points
#' @export
wkb_draw_polypath <- function(wkb, ..., rule = "evenodd") {
  wkcoords_draw_polypath(wkb_coords(wkb, sep_na = TRUE), ..., rule = rule)
  invisible(wkb)
}

#' @rdname wkb_draw_points
#' @export
wkt_draw_polypath <- function(wkt, ..., rule = "evenodd") {
  wkcoords_draw_polypath(wkt_coords(wkt, sep_na = TRUE), ..., rule = rule)
  invisible(wkt)
}

#' @rdname wkb_draw_points
#' @export
wksxp_draw_polypath <- function(wksxp, ..., rule = "evenodd") {
  wkcoords_draw_polypath(wksxp_coords(wksxp, sep_na = TRUE), ..., rule = rule)
  invisible(wksxp)
}

#' @rdname wkb_draw_points
#' @export
wkb_plot_new <- function(wkb, ..., xlab = "", ylab = "", main = deparse(substitute(wkb))) {
  wkranges_plot_new(wkb_ranges(wkb, finite = TRUE), ..., xlab = xlab, ylab = ylab, main = main)
  invisible(wkb)
}

#' @rdname wkb_draw_points
#' @export
wkt_plot_new <- function(wkt, ..., xlab = "", ylab = "", main = deparse(substitute(wkt))) {
  wkranges_plot_new(wkt_ranges(wkt, finite = TRUE), ..., xlab = xlab, ylab = ylab, main = main)
  invisible(wkt)
}

#' @rdname wkb_draw_points
#' @export
wksxp_plot_new <- function(wksxp, ..., xlab = "", ylab = "", main = deparse(substitute(wksxp))) {
  wkranges_plot_new(wksxp_ranges(wksxp, finite = TRUE), ..., xlab = xlab, ylab = ylab, main = main)
  invisible(wksxp)
}

wkcoords_draw_points <- function(coords, ...) {
  graphics::points(coords$x, coords$y, ...)
}

wkcoords_draw_lines <- function(coords, ...) {
  graphics::lines(coords$x, coords$y, ...)
}

wkcoords_draw_polypath <- function(coords, ..., rule = "evenodd") {
  graphics::polypath(coords$x, coords$y, ..., rule = rule)
}

wkranges_plot_new <- function(ranges, ..., xlab = "", ylab = "", main = "") {
  graphics::plot(
    double(), double(),
    xlim = c(ranges$xmin, ranges$xmax),
    ylim = c(ranges$ymin, ranges$ymax),
    xlab = xlab,
    ylab = ylab,
    main = main
  )
}
