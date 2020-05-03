
#' Mark raw vectors as well-known binary
#'
#' @param x A [list()] of [raw()] vectors or `NULL`.
#' @param ... Unused
#'
#' @return A [new_wk_wkb()]
#' @export
#'
#' @examples
#' wkb(wkt_translate_wkb("POINT (20 10)"))
#'
wkb <- function(x) {
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
as_wkb.wk_wkb <- function(x, ...) {
  x
}

#' S3 Details for wk_wkb
#'
#' @param x A (possibly) [wkb()] vector
#' @param ... Unused
#'
#' @export
#'
new_wk_wkb <- function(x) {
  if (typeof(x) != "list" || !is.null(attributes(x))) {
    stop("wkb input must be a list without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkb", "wk_vctr"))
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
format.wk_wkb <- function(x, ...) {
  heads <- lapply(unclass(x), utils::head, 5)
  vapply(heads, function(x) paste0(format(x), collapse = " "), character(1))
}
