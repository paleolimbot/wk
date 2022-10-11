
#' 2D Circle Vectors
#'
#' @param x,y Coordinates of the center
#' @param r Circle radius
#' @param ... Extra arguments passed to `as_crc()`.
#' @inheritParams new_wk_wkb
#'
#' @return A vector along the recycled length of bounds.
#' @export
#'
#' @examples
#' crc(1, 2, 3)
#'
crc <- function(x = double(), y = double(), r = double(), crs = wk_crs_auto()) {
  vec <- new_wk_crc(
    recycle_common(
      x = as.double(x),
      y = as.double(y),
      r = as.double(r)
    ),
    crs = wk_crs_auto_value(x, crs)
  )

  validate_wk_crc(vec)
  vec
}

#' @rdname crc
#' @export
as_crc <- function(x, ...) {
  UseMethod("as_crc")
}

#' @rdname crc
#' @export
as_crc.wk_crc <- function(x, ...) {
  x
}

#' @rdname crc
#' @export
as_crc.matrix <- function(x, ..., crs = NULL) {
  if (ncol(x) == 3) {
    colnames(x) <- c("x", "y", "r")
  }

  as_crc(as.data.frame(x), ..., crs = crs)
}

#' @rdname crc
#' @export
as_crc.data.frame <- function(x, ..., crs = NULL) {
  stopifnot(all(c("x", "y", "r") %in% names(x)))
  new_wk_crc(lapply(x[c("x", "y", "r")], as.double), crs = crs)
}

validate_wk_crc <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(
    identical(names(unclass(x)), c("x", "y", "r"))
  )
  invisible(x)
}

#' S3 details for crc objects
#'
#' @param x A [crc()]
#' @inheritParams new_wk_wkb
#'
#' @export
#'
new_wk_crc <- function(x = list(x = double(), y = double(), r = double()),
                       crs = NULL) {
  structure(x, class = c("wk_crc", "wk_rcrd"), crs = crs)
}

#' @export
format.wk_crc <- function(x, ...) {
  x <- unclass(x)
  sprintf(
    "[%s %s, r = %s]",
    format(x$x, ...), format(x$y, ...),
    format(x$r, ...)
  )
}

#' @export
`[<-.wk_crc` <- function(x, i, value) {
  replacement <- as_crc(value)
  result <- Map("[<-", unclass(x), i, unclass(replacement))
  names(result) <- c("x", "y", "r")
  new_wk_crc(result, crs = wk_crs_output(x, replacement))
}


#' Circle accessors
#'
#' @param x A [crc()] vector
#'
#' @return Components of the [crc()] vector
#' @export
#'
#' @examples
#' x <- crc(1, 2, r = 3)
#' crc_x(x)
#' crc_y(x)
#' crc_r(x)
#' crc_center(x)
#'
crc_x <- function(x) {
  unclass(as_crc(x))$x
}

#' @rdname crc_x
#' @export
crc_y <- function(x) {
  unclass(as_crc(x))$y
}

#' @rdname crc_x
#' @export
crc_center <- function(x) {
  x <- as_crc(x)
  crs <- wk_crs(x)
  x <- unclass(x)
  xy(x$x, x$y, crs = crs)
}

#' @rdname crc_x
#' @export
crc_r <- function(x) {
  unclass(as_crc(x))$r
}
