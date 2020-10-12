
#' Efficient point vectors
#'
#' @param x,y,z,m Coordinate values.
#' @param dims A set containing one or more of `c("x", "y", "z", "m")`.
#' @param ... Passed to methods.
#'
#' @return A vector of coordinate values.
#' @export
#'
#' @examples
#' xy(1:5, 1:5)
#' xyz(1:5, 1:5, 10)
#' xym(1:5, 1:5, 10)
#' xyzm(1:5, 1:5, 10, 12)
#'
xy <- function(x = double(), y = double()) {
  vec <- new_wk_xy(recycle_common(x = as.double(x), y = as.double(y)))
  validate_wk_xy(vec)
  vec
}

#' @rdname xy
#' @export
xyz <- function(x = double(), y = double(), z = double()) {
  vec <- new_wk_xyz(recycle_common(x = as.double(x), y = as.double(y), z = as.double(z)))
  validate_wk_xyz(vec)
  vec
}

#' @rdname xy
#' @export
xym <- function(x = double(), y = double(), m = double()) {
  vec <- new_wk_xym(recycle_common(x = as.double(x), y = as.double(y), m = as.double(m)))
  validate_wk_xym(vec)
  vec
}

#' @rdname xy
#' @export
xyzm <- function(x = double(), y = double(), z = double(), m = double()) {
  vec <- new_wk_xyzm(
    recycle_common(
      x = as.double(x),
      y = as.double(y),
      z = as.double(z),
      m = as.double(m)
    )
  )
  validate_wk_xyzm(vec)
  vec
}

#' @rdname xy
#' @export
xy_dims <- function(x) {
  names(unclass(x))
}

#' @rdname xy
#' @export
as_xy <- function(x, ...) {
  UseMethod("as_xy")
}

#' @rdname xy
#' @export
as_xy.wk_xy <- function(x, ..., dims = NULL) {
  if (is.null(dims)) {
    x
  } else if (setequal(dims, c("x", "y"))) {
    new_wk_xy(fill_missing_dims(unclass(x), c("x", "y"), length(x)))

  } else if (setequal(dims, c("x", "y", "z"))) {
    new_wk_xyz(fill_missing_dims(unclass(x), c("x", "y", "z"), length(x)))

  } else if (setequal(dims, c("x", "y", "m"))) {
    new_wk_xym(fill_missing_dims(unclass(x), c("x", "y", "m"), length(x)))

  } else if (setequal(dims, c("x", "y", "z", "m"))) {
    new_wk_xyzm(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), length(x)))

  } else {
    stop("Unkown dims in as_xy().", call. = FALSE)
  }
}

fill_missing_dims <- function(x, dims, len) {
  missing_dims <- setdiff(dims, names(x))
  x[missing_dims] <- lapply(
    stats::setNames(missing_dims, missing_dims),
    function(x) rep_len(NA_real_, len)
  )
  x[dims]
}

#' S3 details for xy objects
#'
#' @param x A bare named list.
#'
#' @return A vector of class 'wk_xy(zm)'
#' @export
#'
new_wk_xy <- function(x = list(x = double(), y = double())) {
  structure(x, class = c("wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xyz <- function(x = list(x = double(), y = double(), z = double())) {
  structure(x, class = c("wk_xyz", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xym <- function(x = list(x = double(), y = double(), m = double())) {
  structure(x, class = c("wk_xym", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xyzm <- function(x = list(x = double(), y = double(), z = double(), m = double())) {
  structure(x, class = c("wk_xyzm", "wk_xyz", "wk_xym", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
validate_wk_xy <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xyz <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "z")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xym <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "m")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xyzm <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "z", "m")))
  invisible(x)
}

#' @export
format.wk_xy <- function(x, ...) {
  x <- unclass(x)
  sprintf("(%s %s)", format(x$x, ...), format(x$y, ...))
}

#' @export
format.wk_xyz <- function(x, ...) {
  x <- unclass(x)
  sprintf("Z (%s %s %s)", format(x$x, ...), format(x$y, ...), format(x$z, ...))
}

#' @export
format.wk_xym <- function(x, ...) {
  x <- unclass(x)
  sprintf("M (%s %s %s)", format(x$x, ...), format(x$y, ...), format(x$m, ...))
}

#' @export
format.wk_xyzm <- function(x, ...) {
  x <- unclass(x)
  sprintf("ZM (%s %s %s %s)", format(x$x, ...), format(x$y, ...), format(x$z, ...), format(x$m, ...))
}
