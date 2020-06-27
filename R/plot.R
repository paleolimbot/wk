
#' Plot well-known geometry vectors
#'
#' These plot functions are intended to help debug geometry vectors,
#' and are not intended to be high-performance.
#'
#' @param x A [wkt()], [wkb()], or [wksxp()] vector.
#' @param add Should a new plot be created, or should `x` be added to the
#'   existing plot?
#' @param ... Passed to plotting functions for features: [graphics::points()]
#'   for point and multipoint geometries, [graphics::lines()] for linestring
#'   and multilinestring geometries, and [graphics::polypath()] for polygon
#'   and multipolygon geometries.
#' @param bbox The limits of the plot in the form returned by [wksxp_ranges()].
#' @param asp,xlab,ylab Passed to [graphics::plot()]
#' @param rule The rule to use for filling polygons (see [graphics::polypath()])
#'
#' @return `x`, invisibly
#' @export
#'
#' @examples
#' plot(wkt("POINT (30 10)"))
#'
plot.wk_wkt <- function(x, ...,
                        asp = 1, bbox = NULL, xlab = "", ylab = "",
                        rule = "evenodd", add = FALSE) {
  plot_wk(
    x, wkt_ranges, wkt_meta, wkt_coords,
    ...,
    asp = asp, bbox = bbox, xlab = xlab,
    rule = rule, add = add
  )
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_wkb <- function(x, ...,
                        asp = 1, bbox = NULL, xlab = "", ylab = "",
                        rule = "evenodd", add = FALSE) {
  plot_wk(
    x, wkb_ranges, wkb_meta, wkb_coords,
    ...,
    asp = asp, bbox = bbox, xlab = xlab,
    rule = rule, add = add
  )
}

#' @rdname plot.wk_wkt
#' @export
plot.wk_wksxp <- function(x, ...,
                          asp = 1, bbox = NULL, xlab = "", ylab = "",
                          rule = "evenodd", add = FALSE) {
  plot_wk(
    x, wksxp_ranges, wksxp_meta, wksxp_coords,
    ...,
    asp = asp, bbox = bbox, xlab = xlab,
    rule = rule, add = add
  )
}

plot_wk <- function(x, ranges_fun, meta_fun, coords_fun, ...,
                    asp = 1, bbox = NULL, xlab = "", ylab = "",
                    rule = "evenodd", add = FALSE) {
  if (!add) {
    bbox <- unclass(bbox)
    bbox <- bbox %||% ranges_fun(x, finite = TRUE)
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

  plot_add_wk(x, meta_fun, coords_fun, ..., rule = rule)
}

plot_add_wk <- function(x, meta_fun, coords_fun, ..., rule = "evenodd") {
  # evaluate dots, wrap scalar types in a list(), and vectorize
  dots <- list(..., rule = rule)
  is_scalar <- !vapply(dots, vctrs::vec_is, logical(1))
  dots[is_scalar] <- lapply(dots[is_scalar], list)
  dots_tbl <- vctrs::vec_recycle_common(!!!dots, .size = length(x))
  meta <- unclass(meta_fun(x, recursive = FALSE))

  # using for() because the user interrupt is respected in RStudio
  for (i in seq_along(x)) {
    coords <- coords_fun(x[[i]], sep_na = TRUE)[c("x", "y")]
    if (nrow(coords) == 0) {
      next
    }

    dots_item <- lapply(dots_tbl, "[[", i)
    type_id <- meta$type[i]
    args <- c(coords, dots_item)

    if (type_id == 1 || type_id == 4) {
      args$rule <- NULL
      do.call(graphics::points, args)
    } else if (type_id == 2 || type_id == 5) {
      args$rule <- NULL
      do.call(graphics::lines, args)
    } else if (type_id == 3 || type_id == 6) {
      do.call(graphics::polypath, args)
    } else if (type_id == 7) {
      feature_wksxp <- wksxp(unclass(as_wksxp(x[i]))[[1]])
      do.call(
        plot_add_wk,
        c(
          list(feature_wksxp, meta_fun = wksxp_meta, coords_fun = wksxp_coords),
          dots_item
        )
      )
    } else {
      stop("Unknown geometry type", call. = FALSE) # nocov
    }
  }

  invisible(x)
}
