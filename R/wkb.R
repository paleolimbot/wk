
#' Mark lists of raw vectors as well-known binary
#'
#' @param x A [list()] of [raw()] vectors or `NULL`.
#' @inheritParams wkb_translate_wkt
#' @param ... Unused
#'
#' @return A [new_wk_wkb()]
#' @export
#'
#' @examples
#' wkb(wkt_translate_wkb("POINT (20 10)"))
#'
wkb <- function(x = list()) {
  attributes(x) <- NULL
  wkb <- new_wk_wkb(x)
  validate_wk_wkb(x)
  wkb
}

#' @rdname wkb
#' @export
as_wkb <- function(x, ...) {
  UseMethod("as_wkb")
}

#' @rdname wkb
#' @export
as_wkb.character <- function(x, ...) {
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
      )
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
    )
  )
}

#' S3 Details for wk_wkb
#'
#' @param x A (possibly) [wkb()] vector
#'
#' @export
#'
new_wk_wkb <- function(x = list()) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wkb input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkb", "wk_vctr", "geovctr"))
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
