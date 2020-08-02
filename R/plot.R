
#' Plot well-known geometry vectors
#'
#' @inheritParams wkutils::wkt_plot
#'
#' @return The input, invisibly.
#' @importFrom graphics plot
#' @export
#'
#' @examples
#' plot(as_wkt("LINESTRING (0 0, 1 1)"))
#' plot(as_wkb("LINESTRING (0 0, 1 1)"))
#' plot(as_wksxp("LINESTRING (0 0, 1 1)"))
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
plot.wk_wksxp <- function(x, ..., asp = 1, bbox = NULL, xlab = "", ylab = "",
                          rule = "evenodd", add = FALSE) {
  wkutils::wksxp_plot(
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
