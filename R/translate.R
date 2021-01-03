
#' Translate between WKB and WKT
#'
#' @param wkb A `list()` of [raw()] vectors, such as that
#'   returned by `sf::st_as_binary()`.
#' @param wkt A character vector containing well-known text.
#' @param trim Trim unnecessary zeroes in the output?
#' @param precision The rounding precision to use when writing
#'   (number of decimal places).
#' @param endian For WKB writing, 0 for big endian, 1 for little endian.
#'   Defaults to [wk_platform_endian()] (slightly faster).
#' @param include_z,include_m,include_srid Include the
#'   values of the Z and M coordinates and/or SRID
#'   in the output? Use `FALSE` to omit, `TRUE` to include, or `NA` to include
#'   only if present. Note that using `TRUE` may result in an error if there
#'   is no value present in the original.
#' @param buffer_size For WKB writing, the initial buffer size to use for
#'   each feature, in bytes. This will be extended when needed, but if you
#'   are calling this repeatedly with huge geometries, setting this value
#'   to a larger number may result in less copying.
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
#' wkb_translate_wkb(wkb, endian = 0)
#'
wkb_translate_wkt <- function(wkb, include_z = NA, include_m = NA, include_srid = NA,
                              precision = 16, trim = TRUE) {
  cpp_wkb_translate_wkt(
    wkb,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    precision = precision,
    trim = trim
  )
}

#' @rdname wkb_translate_wkt
#' @export
wkb_translate_wkb <- function(wkb, include_z = NA, include_m = NA, include_srid = NA,
                              endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_wkb_translate_wkb(
    wkb,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    endian = endian,
    bufferSize = buffer_size
  )
}

#' @rdname wkb_translate_wkt
#' @export
wkt_translate_wkt <- function(wkt, include_z = NA, include_m = NA, include_srid = NA,
                              precision = 16, trim = TRUE) {
  cpp_wkt_translate_wkt(
    wkt,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    precision = precision,
    trim = trim
  )
}

#' @rdname wkb_translate_wkt
#' @export
wkt_translate_wkb <- function(wkt, include_z = NA, include_m = NA, include_srid = NA,
                              endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_wkt_translate_wkb(
    wkt,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    endian = endian,
    bufferSize = buffer_size
  )
}

# ----- leaving these unexported until the arguments are stable

xyzm_translate_wkt <- function(xyzm, precision = 16, trim = TRUE) {
  cpp_translate_xyzm_wkt(xyzm, precision, trim)
}

xyzm_translate_wkb <- function(xyzm, endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_translate_xyzm_wkb(xyzm, endian, bufferSize = buffer_size)
}

rct_translate_wkt <- function(rct, precision = 16, trim = TRUE) {
  cpp_translate_rct_wkt(rct, precision, trim)
}

rct_translate_wkb <- function(rct, endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_translate_rct_wkb(rct, endian, bufferSize = buffer_size)
}

rct_translate_wksxp <- function(rct) {
  cpp_translate_rct_wksxp(rct)
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
