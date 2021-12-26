
#' Mark lists of raw vectors as well-known binary
#'
#' @param x A [list()] of [raw()] vectors or `NULL`.
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wkb()]
#' @export
#'
#' @examples
#' as_wkb("POINT (20 10)")
#'
wkb <- function(x = list(), crs = wk_crs_auto(), geodesic = FALSE) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  wkb <- new_wk_wkb(x, crs = crs, geodesic = if (isTRUE(geodesic)) TRUE else NULL)
  validate_wk_wkb(wkb)
  wkb
}

#' @rdname wkb
#' @export
parse_wkb <- function(x, crs = wk_crs_auto(), geodesic = FALSE) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  wkb <- new_wk_wkb(x, crs = crs, geodesic = if (isTRUE(geodesic)) TRUE else NULL)
  parse_base(wkb, wkb_problems(wkb))
}

#' @rdname wkb
#' @export
wk_platform_endian <- function() {
  match(.Platform$endian, c("big", "little")) - 1L
}

#' @rdname wkb
#' @export
as_wkb <- function(x, ...) {
  UseMethod("as_wkb")
}

#' @rdname wkb
#' @export
as_wkb.default <- function(x, ...) {
  wk_translate(x, new_wk_wkb(crs = wk_crs_inherit()), ...)
}

#' @rdname wkb
#' @export
as_wkb.character <- function(x, ..., crs = NULL, geodesic = FALSE) {
  as_wkb(wkt(x, crs = crs, geodesic = geodesic), ...)
}

#' @rdname wkb
#' @export
as_wkb.wk_wkb <- function(x, ...) {
  x
}

#' @rdname wkb
#' @export
as_wkb.blob <- function(x, ..., crs = NULL, geodesic = FALSE) {
  as_wkb(wkb(x, crs = crs, geodesic = geodesic), ...)
}

#' @rdname wkb
#' @export
as_wkb.WKB <- function(x, ..., crs = NULL, geodesic = FALSE) {
  as_wkb(wkb(x, crs = crs, geodesic = geodesic), ...)
}

#' S3 Details for wk_wkb
#'
#' @param x A (possibly) [wkb()] vector
#' @param crs A value to be propagated as the CRS for this vector.
#' @inheritParams wk_is_geodesic
#'
#' @export
#'
new_wk_wkb <- function(x = list(), crs = NULL, geodesic = NULL) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wkb input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkb", "wk_vctr"), crs = crs, geodesic = geodesic)
}

#' @rdname new_wk_wkb
#' @export
validate_wk_wkb <- function(x) {
  if (typeof(x) != "list") {
    stop("wkb() must be of type list()", call. = FALSE)
  }

  good_types <- .Call(wk_c_wkb_is_raw_or_null, x)
  if (!all(good_types)) {
    stop("items in wkb input must be raw() or NULL", call. = FALSE)
  }

  if (!inherits(x, "wk_wkb") || !inherits(x, "wk_vctr")) {
    attributes(x) <- NULL
    problems <- wk_problems(new_wk_wkb(x))
  } else {
    problems <- wk_problems(x)
  }

  stop_for_problems(problems)

  invisible(x)
}

#' @rdname new_wk_wkb
#' @export
is_wk_wkb <- function(x) {
  inherits(x, "wk_wkb")
}

#' @export
`[<-.wk_wkb` <- function(x, i, value) {
  replacement <- as_wkb(value)
  crs_out <- wk_crs_output(x, replacement)
  geodesic_out <- wk_is_geodesic_output(x, replacement)
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  attr(x, "geodesic") <- NULL
  new_wk_wkb(x, crs = crs_out, geodesic = if (geodesic_out) TRUE else NULL)
}

#' @export
is.na.wk_wkb <- function(x) {
  .Call(wk_c_wkb_is_na, x)
}

#' @export
format.wk_wkb <- function(x, ...) {
  paste0("<", wkb_format(x), ">")
}

# as far as I can tell, this is the only way to change
# how the object appears in the viewer
#' @export
as.character.wk_wkb <- function(x, ...) {
  format(x, ...)
}
