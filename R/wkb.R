
#' Mark lists of raw vectors as well-known binary
#'
#' @param x A [list()] of [raw()] vectors or `NULL`.
#' @inheritParams wkb_translate_wkt
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wkb()]
#' @export
#'
#' @examples
#' wkb(wkt_translate_wkb("POINT (20 10)"))
#'
wkb <- function(x = list(), crs = NULL) {
  attributes(x) <- NULL
  wkb <- new_wk_wkb(x, crs = crs)
  validate_wk_wkb(x)
  wkb
}

#' @rdname wkb
#' @export
parse_wkb <- function(x) {
  attributes(x) <- NULL
  parse_base(new_wk_wkb(x), wkb_problems(x))
}

#' @rdname wkb
#' @export
as_wkb <- function(x, ...) {
  UseMethod("as_wkb")
}

#' @rdname wkb
#' @export
as_wkb.character <- function(x, ..., crs = NULL) {
  as_wkb(wkt(x), ...)
}

#' @rdname wkb
#' @export
as_wkb.wk_wkb <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                          endian = NULL) {
  if (is.null(include_z) && is.null(include_m) && is.null(include_srid) && is.null(endian)) {
    x
  } else {
    new_wk_wkb(
      wkb_translate_wkb(
        x,
        include_z = include_z %||% NA,
        include_m = include_m %||% NA,
        include_srid = include_srid %||% NA,
        endian = endian %||% wk_platform_endian()
      ),
      crs = attr(x, "crs", exact = TRUE)
    )
  }
}

#' @rdname wkb
#' @export
as_wkb.wk_wkt <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                          endian = NULL) {
  new_wk_wkb(
    wkt_translate_wkb(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA,
      endian = endian %||% wk_platform_endian()
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname wkb
#' @export
as_wkb.wk_wksxp <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                            endian = NULL) {
  new_wk_wkb(
    wksxp_translate_wkb(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA,
      endian = endian %||% wk_platform_endian()
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname wkb
#' @export
as_wkb.blob <- function(x, ...) {
  as_wkb(wkb(x), ...)
}

#' @rdname wkb
#' @export
as_wkb.WKB <- function(x, ...) {
  as_wkb(wkb(x), ...)
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
  x <- unclass(x)
  value <- as_wkb(value)
  x[i] <- value
  x_crs <- attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_wk_wkb(
    x,
    crs = wk_crs_output(
      x_crs,
      attr(value, "crs", exact = TRUE)
    )
  )
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
