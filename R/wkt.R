
#' Mark character vectors as well-known text
#'
#' @param x A [character()] vector containing well-known text.
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wkt()]
#' @export
#'
#' @examples
#' wkt("POINT (20 10)")
#'
wkt <- function(x = character(), crs = wk_crs_auto(), geodesic = FALSE) {
  x <- as.character(x)
  crs <- wk_crs_auto_value(x, crs)
  wkt <- new_wk_wkt(x, crs = crs, geodesic = if (isTRUE(geodesic)) TRUE else NULL)
  validate_wk_wkt(wkt)
  wkt
}

#' @rdname wkt
#' @export
parse_wkt <- function(x, crs = wk_crs_auto(), geodesic = FALSE) {
  x <- as.character(x)
  crs <- wk_crs_auto_value(x, crs)
  wkt <- new_wk_wkt(x, crs = crs, geodesic = if (isTRUE(geodesic)) TRUE else NULL)
  parse_base(wkt, wk_problems(wkt))
}

#' @rdname wkt
#' @export
as_wkt <- function(x, ...) {
  UseMethod("as_wkt")
}

#' @rdname wkt
#' @export
as_wkt.default <- function(x, ...) {
  wk_translate(
    x,
    new_wk_wkt(crs = wk_crs_inherit(), geodesic = if (wk_is_geodesic(x)) TRUE)
  )
}

#' @rdname wkt
#' @export
as_wkt.character <- function(x, ..., crs = NULL, geodesic = FALSE) {
  wkt(x, crs = crs, geodesic = geodesic)
}

#' @rdname wkt
#' @export
as_wkt.wk_wkt <- function(x, ...) {
  x
}

#' S3 Details for wk_wkt
#'
#' @param x A (possibly) [wkt()] vector
#' @inheritParams new_wk_wkb
#'
#' @export
#'
new_wk_wkt <- function(x = character(), crs = NULL, geodesic = NULL) {
  if (typeof(x) != "character" || !is.null(attributes(x))) {
    stop("wkt input must be a character() without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkt", "wk_vctr"), crs = crs, geodesic = geodesic)
}

#' @rdname new_wk_wkt
#' @export
is_wk_wkt <- function(x) {
  inherits(x, "wk_wkt")
}

#' @rdname new_wk_wkt
#' @export
validate_wk_wkt <- function(x) {
  if (typeof(x) != "character") {
    stop("wkt() must be of type character()", call. = FALSE)
  }

  # See #123...validate_wk_wkt() is used in CRAN s2 on a raw character vector
  if (!inherits(x, "wk_wkt") || !inherits(x, "wk_vctr")) {
    # stop('wkt() must inherit from c("wk_wkt", "wk_vctr")', call. = FALSE)
    attributes(x) <- NULL
    problems <- wk_problems(new_wk_wkt(x))
  } else {
    problems <- wk_problems(x)
  }

  stop_for_problems(problems)

  invisible(x)
}

#' @export
`[<-.wk_wkt` <- function(x, i, value) {
  replacement <- as_wkt(value)
  crs_out <- wk_crs_output(x, replacement)
  geodesic_out <- wk_is_geodesic_output(x, replacement)
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  attr(x, "geodesic") <- NULL
  new_wk_wkt(x, crs = crs_out, geodesic = if (geodesic_out) TRUE else NULL)
}

#' @export
format.wk_wkt <- function(x, ..., max_coords = 6) {
  wk_format(x, max_coords = max_coords)
}

#' @export
as.character.wk_wkt <- function(x, ...) {
  attr(x, "crs") <- NULL
  unclass(x)
}
