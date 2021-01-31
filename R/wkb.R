
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
#' wkb(wkt_translate_wkb("POINT (20 10)"))
#'
wkb <- function(x = list(), crs = wk_crs_auto()) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  wkb <- new_wk_wkb(x, crs = crs)
  validate_wk_wkb(x)
  wkb
}

#' @rdname wkb
#' @export
parse_wkb <- function(x, crs = wk_crs_auto()) {
  crs <- wk_crs_auto_value(x, crs)
  attributes(x) <- NULL
  parse_base(new_wk_wkb(x, crs = crs), wkb_problems(x))
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
as_wkb.character <- function(x, ..., crs = NULL) {
  as_wkb(wkt(x, crs = crs), ...)
}

#' @rdname wkb
#' @export
as_wkb.wk_wkb <- function(x, ...) {
  x
}

#' @rdname wkb
#' @export
as_wkb.blob <- function(x, ..., crs = NULL) {
  as_wkb(wkb(x, crs = crs), ...)
}

#' @rdname wkb
#' @export
as_wkb.WKB <- function(x, ..., crs = NULL) {
  as_wkb(wkb(x, crs = crs), ...)
}

#' S3 Details for wk_wkb
#'
#' @param x A (possibly) [wkb()] vector
#' @param crs A value to be propagated as the CRS for this vector.
#'
#' @export
#'
new_wk_wkb <- function(x = list(), crs = NULL) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wkb input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkb", "wk_vctr"), crs = crs)
}

#' @rdname new_wk_wkb
#' @export
validate_wk_wkb <- function(x) {
  types <- vapply(unclass(x), typeof, character(1))
  good_types <- types %in% c("raw", "NULL")
  if (any(!good_types)) {
    stop("items in wkb input must be raw() or NULL", call. = FALSE)
  }

  problems <- wkb_problems(x)
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
  x <- unclass(x)
  x[i] <- replacement
  attr(x, "crs") <- NULL
  new_wk_wkb(x, crs = crs_out)
}

#' @export
is.na.wk_wkb <- function(x) {
  vapply(unclass(x), is.null, logical(1))
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
