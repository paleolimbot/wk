
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
rct <- function(xmin = double(), ymin = double(), xmax = double(), ymax = double(), crs = wk_crs_auto()) {
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
  new_wk_rct(lapply(x[c("xmin", "ymin", "xmax", "ymax")], as.double), crs = crs)
}

validate_wk_rct <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(
    identical(names(unclass(x)), c("xmin", "ymin", "xmax", "ymax"))
  )
  invisible(x)
}

#' @export
as_wkt.wk_rct <- function(x, ...) {
  new_wk_wkt(rct_translate_wkt(x, ...), crs = attr(x, "crs", exact = TRUE))
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
  new_wk_rct(result, crs = wk_crs_output(x, replacement))
}
