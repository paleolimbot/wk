
#' Translate between WKB and WKT
#'
#' @param wkb A `list()` of [raw()] vectors, such as that
#'   returned by `sf::st_as_binary()`.
#' @param wkt A character vector containing well-known text.
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#' @param ... Used to keep backward compatibility with previous
#'   versions of these functions.
#'
#' @return `*_translate_wkt()` returns a character vector of
#'   well-known text; `*_translate_wkb()` returns a list
#'   of raw vectors. Unlike [as_wkb()], [as_wkt()],
#'   these functions do not attach a class to the output.
#'
#' @export
#'
#' @examples
#' # translate between WKT and WKB
#' (wkb <- wkt_translate_wkb("POINT (30 10)"))
#' wkb_translate_wkt(wkb)
#'
#' # some basic creation options are also available
#' wkt_translate_wkt("POINT (30 10)", trim = FALSE)
#'
wkb_translate_wkt <- function(wkb, ..., precision = 16, trim = TRUE) {
  wk_handle.wk_wkb(wkb, wkt_writer(precision, trim))
}

#' @rdname wkb_translate_wkt
#' @export
wkb_translate_wkb <- function(wkb, ...) {
  wk_handle.wk_wkb(wkb, wkb_writer())
}

#' @rdname wkb_translate_wkt
#' @export
wkt_translate_wkt <- function(wkt, ..., precision = 16, trim = TRUE) {
  cpp_wkt_translate_wkt(
    wkt,
    includeZ = NA,
    includeM = NA,
    includeSRID = NA,
    precision = precision,
    trim = trim
  )
}

#' @rdname wkb_translate_wkt
#' @export
wkt_translate_wkb <- function(wkt, ...) {
  cpp_wkt_translate_wkb(
    wkt,
    includeZ = NA,
    includeM = NA,
    includeSRID = NA,
    endian = wk_platform_endian(),
    bufferSize = 2048
  )
}

# ----- leaving these unexported until the arguments are stable

xyzm_translate_wkt <- function(xyzm, precision = 16, trim = TRUE) {
  cpp_translate_xyzm_wkt(xyzm, precision, trim)
}

xyzm_translate_wkb <- function(xyzm) {
  cpp_translate_xyzm_wkb(xyzm, wk_platform_endian(), bufferSize = 2048)
}

rct_translate_wkt <- function(rct, precision = 16, trim = TRUE) {
  cpp_translate_rct_wkt(rct, precision, trim)
}

rct_translate_wkb <- function(rct) {
  cpp_translate_rct_wkb(rct, wk_platform_endian(), bufferSize = 2048)
}

wkt_translate_xyzm <- function(wkt, include_z = NA, include_m = NA) {
  xyzm_trim(cpp_translate_wkt_xyzm(wkt), include_z, include_m)
}

wkb_translate_xyzm <- function(wkb, include_z = NA, include_m = NA) {
  xyzm_trim(cpp_translate_wkb_xyzm(wkb), include_z, include_m)
}

xyzm_trim <- function(result, include_z, include_m) {
  if (identical(include_z, NA) && all(is.na(result$z))) {
    result$z <- NULL
  } else if (identical(include_z, FALSE)) {
    result$z <- NULL
  }

  if (identical(include_m, NA) && all(is.na(result$m))) {
    result$m <- NULL
  } else if (identical(include_m, FALSE)) {
    result$m <- NULL
  }

  result
}

#' @rdname wkb_translate_wkt
#' @export
wk_platform_endian <- function() {
  match(.Platform$endian, c("big", "little")) - 1L
}
