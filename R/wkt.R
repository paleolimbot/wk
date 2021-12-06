
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
wkt <- function(x = character(), crs = wk_crs_auto(), is_geodesic = FALSE) {
  x <- as.character(x)
  crs <- wk_crs_auto_value(x, crs)
  wkt <- new_wk_wkt(x, crs = crs, is_geodesic = if (isTRUE(is_geodesic)) TRUE else NULL)
  validate_wk_wkt(x)
  wkt
}

#' @rdname wkt
#' @export
parse_wkt <- function(x, crs = wk_crs_auto()) {
  x <- as.character(x)
  crs <- wk_crs_auto_value(x, crs)
  parse_base(new_wk_wkt(x, crs = crs), wkt_problems(x))
}

#' @rdname wkt
#' @export
as_wkt <- function(x, ...) {
  UseMethod("as_wkt")
}

#' @rdname wkt
#' @export
as_wkt.default <- function(x, ...) {
  wk_translate(x, new_wk_wkt(crs = wk_crs_inherit()))
}

#' @rdname wkt
#' @export
as_wkt.character <- function(x, ..., crs = NULL) {
  wkt(x, crs = crs)
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
new_wk_wkt <- function(x = character(), crs = NULL, is_geodesic = NULL) {
  if (typeof(x) != "character" || !is.null(attributes(x))) {
    stop("wkt input must be a character() without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkt", "wk_vctr"), crs = crs, is_geodesic = is_geodesic)
}

#' @rdname new_wk_wkt
#' @export
is_wk_wkt <- function(x) {
  inherits(x, "wk_wkt")
}

#' @rdname new_wk_wkt
#' @export
validate_wk_wkt <- function(x) {
  problems <- wkt_problems(x)
  stop_for_problems(problems)

  invisible(x)
}

#' @export
`[<-.wk_wkt` <- function(x, i, value) {
  replacement <- as_wkt(value)
  crs_out <- wk_crs_output(x, replacement)
  is_geodesic_out <- wk_geodesic_output(x, replacement)
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  new_wk_wkt(x, crs = crs_out, is_geodesic = if (is_geodesic_out) TRUE else NULL)
}

#' @export
format.wk_wkt <- function(x, ..., max_coords = 3) {
  formatted <- wkt_format(x, max_coords = max_coords)
  formatted[is.na(formatted)] <- "<NA>"
  formatted
}

#' @export
as.character.wk_wkt <- function(x, ...) {
  attr(x, "crs") <- NULL
  unclass(x)
}
