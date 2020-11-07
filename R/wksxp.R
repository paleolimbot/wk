
#' Mark lists as well-known "S" expressions
#'
#' @details
#' The "wksxp" format is experimental, but was written as a way to
#' make it possible for packages to generate [wkb()] vectors without
#' needing to use C++. The format represents geometries as following:
#'
#' - points are matrices with zero or one row
#' - linestrings are matrices (one row per point)
#' - polygons are lists of matrices (one matrix per ring)
#' - multi (point, linestring, polygon) types are lists
#'   of the simple types (without any meta information)
#' - collections are lists of any type (must contain meta)
#'
#' Any geometry that isn't in a multi type must have meta information
#' encoded as attributes. The attribures that are used are:
#'
#' - `class`: "wk_(point|linestring|...)
#' - `has_z`: use `TRUE` if there is a Z coordinate
#'    (may be omitted if false)
#' - `has_m`: use `TRUE` if there is an M coordinate
#'    (may be omitted if false)
#'
#' This is similar to the `sf::st_sfc()` format, but the formats aren't
#' interchangeable.
#'
#' @param x A [list()] features (see details)
#' @inheritParams wkb_translate_wkt
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wksxp()]
#' @export
#'
#' @examples
#' wksxp(wkt_translate_wksxp("POINT (20 10)"))
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
parse_wksxp <- function(x, crs = wk_crs_auto()) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  parse_base(new_wk_wksxp(x, crs = crs), wksxp_problems(x))
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
as_wksxp.wk_wksxp <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL) {
  if (is.null(include_z) && is.null(include_m) && is.null(include_srid)) {
    x
  } else {
    new_wk_wksxp(
      wksxp_translate_wksxp(
        x,
        include_z = include_z %||% NA,
        include_m = include_m %||% NA,
        include_srid = include_srid %||% NA
      ),
      crs = attr(x, "crs", exact = TRUE)
    )
  }
}

#' @rdname wksxp
#' @export
as_wksxp.wk_wkt <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL) {
  new_wk_wksxp(
    wkt_translate_wksxp(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname wksxp
#' @export
as_wksxp.wk_wkb <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL) {
  new_wk_wksxp(
    wkb_translate_wksxp(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname wkb
#' @export
as_wksxp.blob <- function(x, ..., crs = NULL) {
  as_wksxp(wkb(x, crs = crs), ...)
}

#' @rdname wkb
#' @export
as_wksxp.WKB <- function(x, ..., crs = NULL) {
  as_wksxp(wkb(x, crs = crs), ...)
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
  # types are checked in the parser
  problems <- wksxp_problems(x)
  stop_for_problems(problems)

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

#' @export
format.wk_wksxp <- function(x, ...) {
  paste0("<", wksxp_format(x), ">")
}

# as far as I can tell, this is the only way to change
# how the object appears in the viewer
#' @export
as.character.wk_wksxp <- function(x, ...) {
  format(x, ...)
}
