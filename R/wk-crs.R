
#' Set and get vector CRS
#'
#' The wk package doesn't operate on CRS objects, but does propagate them
#' through subsetting and concatenation. A CRS object can be any R object,
#' and x can be any object whose 'crs' attribute carries a CRS. These functions
#' are S3 generics to keep them from being used
#' on objects that do not use this system of CRS propagation.
#'
#' @param x,... Objects whose "crs" attribute is used to carry a CRS.
#' @param crs,value An object that can be interpreted as a CRS
#'
#' @export
#'
wk_crs <- function(x) {
  UseMethod("wk_crs")
}

#' @rdname wk_crs
#' @export
wk_crs.wk_vctr <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @rdname wk_crs
#' @export
wk_crs.wk_rcrd <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @rdname wk_crs
#' @export
`wk_crs<-` <- function(x, value) {
  wk_set_crs(x, value)
}

#' @rdname wk_crs
#' @export
wk_set_crs <- function(x, crs) {
  UseMethod("wk_set_crs")
}

#' @export
wk_set_crs.wk_vctr <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @export
wk_set_crs.wk_rcrd <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @rdname wk_crs
#' @export
wk_crs_output <- function(...) {
  dots <- list(...)
  crs <- lapply(dots, wk_crs)
  Reduce(wk_crs2, crs)
}

wk_crs2 <- function(x, y) {
  if (inherits(y, "wk_crs_inherit")) {
    x
  } else if (inherits(x, "wk_crs_inherit")) {
    y
  } else if (wk_crs_equal(x, y)) {
    x
  } else {
    stop(sprintf("CRS objects '%s' and '%s' are not equal.", format(x), format(y)), call. = FALSE)
  }
}

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

#' Special CRS values
#'
#' The CRS handling in the wk package requires two sentinel CRS values.
#' The first, [wk_crs_inherit()], signals that the vector should inherit
#' a CRS of another vector if combined. This is useful for empty, `NULL`,
#' and/or zero-length geometries. The second, [wk_crs_auto()], is used
#' as the default argument of `crs` for constructors so that zero-length
#' geometries are assigned a CRS of `wk_crs_inherit()` by default.
#'
#' @param x A raw input to a construuctor whose length and crs attributte
#'   is used to determine the default CRS returned by [wk_crs_auto()].
#' @param crs A value for the coordinate reference system supplied by
#'   the user.
#'
#' @export
#'
#' @examples
#' wk_crs_auto_value(list(), wk_crs_auto())
#' wk_crs_auto_value(list(), 1234)
#' wk_crs_auto_value(list(NULL), wk_crs_auto())
#'
wk_crs_inherit <- function() {
  structure(list(), class = "wk_crs_inherit")
}

#' @rdname wk_crs_inherit
#' @export
wk_crs_auto <- function() {
  structure(list(), class = "wk_crs_auto")
}

#' @rdname wk_crs_inherit
#' @export
wk_crs_auto_value <- function(x, crs) {
  if (inherits(crs, "wk_crs_auto")) {
    if (length(x) == 0) wk_crs_inherit() else attr(x, "crs", exact = TRUE)
  } else {
    crs
  }
}

#' @export
format.wk_crs_inherit <- function(x, ...) {
  format("wk_crs_inherit()", ...)
}

#' @export
print.wk_crs_inherit <- function(x, ...) {
  cat("<wk_crs_inherit>\n")
}
