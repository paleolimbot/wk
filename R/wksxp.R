
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
#' interchangable.
#'
#' @param x A [list()] features (see details)
#' @inheritParams wkb_translate_wkt
#' @param ... Unused
#'
#' @return A [new_wk_wksxp()]
#' @export
#'
#' @examples
#' wksxp(wkt_translate_wksxp("POINT (20 10)"))
#'
wksxp <- function(x = list()) {
  attributes(x) <- NULL
  wksxp <- new_wk_wksxp(x)
  validate_wk_wksxp(x)
  wksxp
}

#' @rdname wksxp
#' @export
parse_wksxp <- function(x) {
  attributes(x) <- NULL
  parse_base(new_wk_wksxp(x), wksxp_problems(x))
}

#' @rdname wksxp
#' @export
as_wksxp <- function(x, ...) {
  UseMethod("as_wksxp")
}

#' @rdname wksxp
#' @export
as_wksxp.default <- function(x, ...) {
  as_wksxp(as_wkb(x), ...)
}

#' @rdname wksxp
#' @export
as_wksxp.character <- function(x, ...) {
  as_wksxp(wkt(x), ...)
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
      )
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
    )
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
    )
  )
}

#' @rdname wkb
#' @export
as_wksxp.blob <- function(x, ...) {
  as_wksxp(wkb(x), ...)
}

#' @rdname wkb
#' @export
as_wksxp.WKB <- function(x, ...) {
  as_wksxp(wkb(x), ...)
}

#' S3 Details for wk_wksxp
#'
#' @param x A (possibly) [wksxp()] vector
#'
#' @export
#'
new_wk_wksxp <- function(x = list()) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wksxp input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wksxp", "wk_vctr"))
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
  x <- unclass(x)
  x[i] <- as_wksxp(value)
  new_wk_wksxp(x)
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
