
#' Mark character vectors as well-known text
#'
#' @param x A [character()] vector containing well-known text.
#' @inheritParams wkb_translate_wkt
#' @inheritParams new_wk_wkb
#' @param ... Unused
#'
#' @return A [new_wk_wkt()]
#' @export
#'
#' @examples
#' wkt("POINT (20 10)")
#'
wkt <- function(x = character(), crs = NULL) {
  x <- as.character(x)
  attributes(x) <- NULL
  wkt <- new_wk_wkt(x, crs = crs)
  validate_wk_wkt(x)
  wkt
}

#' @rdname wkt
#' @export
parse_wkt <- function(x, crs = NULL) {
  x <- as.character(x)
  attributes(x) <- NULL
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
  as_wkt(as_wksxp(x), ...)
}

#' @rdname wkt
#' @export
as_wkt.character <- function(x, ..., crs = NULL) {
  wkt(x, crs = crs)
}

#' @rdname wkt
#' @export
as_wkt.wk_wkt <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                          precision = NULL, trim = NULL) {
  if (is.null(include_z) && is.null(include_m) && is.null(include_srid) &&
      is.null(precision) && is.null(trim)) {
    x
  } else {
    new_wk_wkt(
      wkt_translate_wkt(
        x,
        include_z = include_z %||% NA,
        include_m = include_m %||% NA,
        include_srid = include_srid %||% NA,
        precision = precision %||% 16,
        trim = trim %||% TRUE
      ),
      crs = attr(x, "crs", exact = TRUE)
    )
  }
}

#' @rdname wkt
#' @export
as_wkt.wk_wkb <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                          precision = NULL, trim = NULL) {
  new_wk_wkt(
    wkb_translate_wkt(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA,
      precision = precision %||% 16,
      trim = trim %||% TRUE
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' @rdname wkt
#' @export
as_wkt.wk_wksxp <- function(x, ..., include_z = NULL, include_m = NULL, include_srid = NULL,
                            precision = NULL, trim = NULL) {
  new_wk_wkt(
    wksxp_translate_wkt(
      x,
      include_z = include_z %||% NA,
      include_m = include_m %||% NA,
      include_srid = include_srid %||% NA,
      precision = precision %||% 16,
      trim = trim %||% TRUE
    ),
    crs = attr(x, "crs", exact = TRUE)
  )
}

#' S3 Details for wk_wkt
#'
#' @param x A (possibly) [wkt()] vector
#' @inheritParams new_wk_wkb
#'
#' @export
#'
new_wk_wkt <- function(x = character(), crs = NULL) {
  if (typeof(x) != "character" || !is.null(attributes(x))) {
    stop("wkt input must be a character() without attributes",  call. = FALSE)
  }

  structure(x, class = c("wk_wkt", "wk_vctr"), crs = crs)
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
  x <- unclass(x)
  value <- as_wkt(value)
  x[i] <- value
  x_crs <- attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_wk_wkt(
    x,
    crs = wk_crs_output(
      x_crs,
      attr(value, "crs", exact = TRUE)
    )
  )
}

#' @export
format.wk_wkt <- function(x, ..., max_coords = 3) {
  formatted <- wkt_format(x, max_coords = max_coords)
  formatted[is.na(formatted)] <- "<NA>"
  formatted
}

#' @export
as.character.wk_wkt <- function(x, ...) {
  unclass(x)
}
