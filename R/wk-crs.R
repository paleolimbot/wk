
#' Compare CRS objects
#'
#' The [wk_crs_equal()] function uses special S3 dispatch on [wk_crs_equal_generic()]
#' to evaluate whether or not two CRS values can be considered equal. When implementing
#' [wk_crs_equal_generic()], every attempt should be made to make `wk_crs_equal(x, y)`
#' and `wk_crs_equal(y, x)` return identically.
#'
#' @param x,y Objects stored in the `crs` attribute of a vector.
#' @param ... Unused
#'
#' @return `TRUE` if `x` and `y` can be considered equal, `FALSE` otherwise.
#' @export
#'
wk_crs_equal <- function(x, y) {
  if (is.object(y)) {
    wk_crs_equal_generic(y, x)
  } else {
    wk_crs_equal_generic(x, y)
  }
}

#' @rdname wk_crs_equal
#' @export
wk_crs_output <- function(x, y) {
  if (inherits(y, "wk_crs_inherit")) {
    x
  } else if (inherits(x, "wk_crs_inherit")) {
    y
  } else if (wk_crs_equal(x, y)) {
    x
  } else {
    stop(sprintf("Can't compare CRS objects '%s' and '%s'.", x, y), call. = FALSE)
  }
}

#' @rdname wk_crs_equal
#' @export
wk_crs_inherit <- function() {
  structure(list(), class = "wk_crs_inherit")
}

#' @rdname wk_crs_equal
#' @export
wk_crs_equal_generic <- function(x, y, ...) {
  UseMethod("wk_crs_equal_generic")
}

#' @export
wk_crs_equal_generic.default <- function(x, y, ...) {
  identical(x, y)
}

#' @export
wk_crs_equal_generic.integer <- function(x, y, ...) {
  isTRUE(x == y)
}

#' @export
wk_crs_equal_generic.double <- function(x, y, ...) {
  isTRUE(x == y)
}
