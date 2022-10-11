
#' 2D rectangle vectors
#'
#' @param xmin,ymin,xmax,ymax Rectangle bounds.
#' @param x An object to be converted to a [rct()].
#' @param ... Extra arguments passed to `as_rct()`.
#' @inheritParams new_wk_wkb
#'
#' @return A vector along the recycled length of bounds.
#' @export
#'
#' @examples
#' rct(1, 2, 3, 4)
#'
rct <- function(xmin = double(), ymin = double(), xmax = double(), ymax = double(),
                crs = wk_crs_auto()) {
  vec <- new_wk_rct(
    recycle_common(
      xmin = as.double(xmin),
      ymin = as.double(ymin),
      xmax = as.double(xmax),
      ymax = as.double(ymax)
    ),
    crs = wk_crs_auto_value(xmin, crs)
  )

  validate_wk_rct(vec)
  vec
}

#' @rdname rct
#' @export
as_rct <- function(x, ...) {
  UseMethod("as_rct")
}

#' @rdname rct
#' @export
as_rct.wk_rct <- function(x, ...) {
  x
}

#' @rdname rct
#' @export
as_rct.matrix <- function(x, ..., crs = NULL) {
  if (ncol(x) == 4) {
    colnames(x) <- c("xmin", "ymin", "xmax", "ymax")
  }

  as_rct(as.data.frame(x), ..., crs = crs)
}

#' @rdname rct
#' @export
as_rct.data.frame <- function(x, ..., crs = NULL) {
  stopifnot(all(c("xmin", "ymin", "xmax", "ymax") %in% names(x)))
  new_wk_rct(
    lapply(x[c("xmin", "ymin", "xmax", "ymax")], as.double),
    crs = crs
  )
}

validate_wk_rct <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(
    identical(names(unclass(x)), c("xmin", "ymin", "xmax", "ymax"))
  )
  invisible(x)
}

#' S3 details for rct objects
#'
#' @param x A [rct()]
#' @inheritParams new_wk_wkb
#'
#' @export
#'
new_wk_rct <- function(x = list(xmin = double(), ymin = double(), xmax = double(), ymax = double()),
                       crs = NULL) {
  structure(x, class = c("wk_rct", "wk_rcrd"), crs = crs)
}

#' @export
format.wk_rct <- function(x, ...) {
  x <- unclass(x)
  sprintf(
    "[%s %s %s %s]",
    format(x$xmin, ...), format(x$ymin, ...),
    format(x$xmax, ...), format(x$ymax, ...)
  )
}

#' @export
`[<-.wk_rct` <- function(x, i, value) {
  replacement <- as_rct(value)
  result <- Map("[<-", unclass(x), i, unclass(replacement))
  names(result) <- c("xmin", "ymin", "xmax", "ymax")
  new_wk_rct(
    result,
    crs = wk_crs_output(x, replacement)
  )
}

#' Rectangle accessors and operators
#'
#' @param x,y [rct()] vectors
#'
#' @return
#'   - `rct_xmin()`, `rct_xmax()`, `rct_ymin()`, and `rct_ymax()` return
#'     the components of the [rct()].
#' @export
#'
#' @examples
#' x <- rct(0, 0, 10, 10)
#' y <- rct(5, 5, 15, 15)
#'
#' rct_xmin(x)
#' rct_ymin(x)
#' rct_xmax(x)
#' rct_ymax(x)
#' rct_height(x)
#' rct_width(x)
#' rct_intersects(x, y)
#' rct_intersection(x, y)
#' rct_contains(x, y)
#' rct_contains(x, rct(4, 4, 6, 6))
#'
rct_xmin <- function(x) {
  x <- as_rct(x)
  unclass(x)$xmin
}

#' @rdname rct_xmin
#' @export
rct_ymin <- function(x) {
  x <- as_rct(x)
  unclass(x)$ymin
}

#' @rdname rct_xmin
#' @export
rct_xmax <- function(x) {
  x <- as_rct(x)
  unclass(x)$xmax
}

#' @rdname rct_xmin
#' @export
rct_ymax <- function(x) {
  x <- as_rct(x)
  unclass(x)$ymax
}

#' @rdname rct_xmin
#' @export
rct_width <- function(x) {
  x <- as_rct(x)
  x <- unclass(x)
  x$xmax - x$xmin
}

#' @rdname rct_xmin
#' @export
rct_height <- function(x) {
  x <- as_rct(x)
  x <- unclass(x)
  x$ymax - x$ymin
}

#' @rdname rct_xmin
#' @export
rct_intersects <- function(x, y) {
  x <- as_rct(x)
  y <- as_rct(y)
  wk_crs_output(x, y)

  x <- unclass(x)
  y <- unclass(y)

  limits <- list(
    xmin = pmax(x$xmin, y$xmin),
    xmax = pmin(x$xmax, y$xmax),
    ymin = pmax(x$ymin, y$ymin),
    ymax = pmin(x$ymax, y$ymax)
  )

  (limits$xmax >= limits$xmin) & (limits$ymax >= limits$ymin)
}

#' @rdname rct_xmin
#' @export
rct_contains <- function(x, y) {
  x <- as_rct(x)
  y <- wk_envelope(y)
  wk_crs_output(x, y)

  x <- unclass(x)
  y <- unclass(y)

  (y$xmin >= x$xmin) &
    (y$xmax <= x$xmax) &
    (y$ymin >= x$ymin) &
    (y$ymax <= x$ymax)
}

#' @rdname rct_xmin
#' @export
rct_intersection <- function(x, y) {
  x <- as_rct(x)
  y <- as_rct(y)
  crs <- wk_crs_output(x, y)

  x <- unclass(x)
  y <- unclass(y)

  limits <- list(
    xmin = pmax(x$xmin, y$xmin),
    ymin = pmax(x$ymin, y$ymin),
    xmax = pmin(x$xmax, y$xmax),
    ymax = pmin(x$ymax, y$ymax)
  )

  any_na <- Reduce("|", lapply(limits, is.na))
  not_valid <- any_na | !((limits$xmax >= limits$xmin) & (limits$ymax >= limits$ymin))
  limits$xmin[not_valid] <- NA_real_
  limits$xmax[not_valid] <- NA_real_
  limits$ymin[not_valid] <- NA_real_
  limits$ymax[not_valid] <- NA_real_

  new_wk_rct(limits, crs = crs)
}
