
#' Mark lists as well-known "S" expressions
#'
#' @details
#' The "wksxp" format waw an attempt to provide an R-native format
#' that could losslessly store WKT and WKB. This is now deprecated
#' and will be removed in a future version.
#'
#' @param x A [list()] features (see details)
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wksxp()]
#' @export
#'
wksxp <- function(x = list(), crs = wk_crs_auto()) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  wksxp <- new_wk_wksxp(x, crs = crs)
  validate_wk_wksxp(x)
  wksxp
}

#' @rdname wksxp
#' @export
as_wksxp <- function(x, ...) {
  UseMethod("as_wksxp")
}

#' @rdname wksxp
#' @export
as_wksxp.default <- function(x, ..., crs = NULL) {
  as_wksxp(as_wkb(x, crs = crs), ...)
}

#' @rdname wksxp
#' @export
as_wksxp.character <- function(x, ..., crs = NULL) {
  as_wksxp(wkt(x, crs = crs), ...)
}

#' @rdname wksxp
#' @export
as_wksxp.wk_wksxp <- function(x, ...) {
  x
}

#' S3 Details for wk_wksxp
#'
#' @param x A (possibly) [wksxp()] vector
#' @inheritParams new_wk_wkb
#'
#' @export
#'
new_wk_wksxp <- function(x = list(), crs = NULL) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wksxp input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wksxp", "wk_vctr"), crs = crs)
}

#' @rdname new_wk_wksxp
#' @export
is_wk_wksxp <- function(x) {
  inherits(x, "wk_wksxp")
}

#' @rdname new_wk_wksxp
#' @export
validate_wk_wksxp <- function(x) {
  # deprecated; don't check!
  invisible(x)
}

#' @export
`[<-.wk_wksxp` <- function(x, i, value) {
  replacement <- as_wksxp(value)
  crs_out <- wk_crs_output(x, replacement)
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  new_wk_wksxp(x, crs = crs_out)
}

#' @export
is.na.wk_wksxp <- function(x) {
  vapply(unclass(x), is.null, logical(1))
}

# as far as I can tell, this is the only way to change
# how the object appears in the viewer
#' @export
as.character.wk_wksxp <- function(x, ...) {
  format(x, ...)
}
