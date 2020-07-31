
#' Translate between WKB and WKT
#'
#' @param wkb A `list()` of [raw()] vectors, such as that
#'   returned by `sf::st_as_binary()`.
#' @param wkt A character vector containing well-known text.
#' @param wksxp A `list()` of classed objects
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
#'   of raw vectors, and `*_translate_wksxp()` returns an unclassed
#'   list of [wksxp()] geometries. Unlike [as_wkb()], [as_wkt()], and
#'   [as_wksxp()], these functions do not attach
#'   a class to the output.
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
wkb_translate_wksxp <- function(wkb, include_z = NA, include_m = NA, include_srid = NA) {
  cpp_wkb_translate_wksxp(
    wkb,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid
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

#' @rdname wkb_translate_wkt
#' @export
wkt_translate_wksxp <- function(wkt, include_z = NA, include_m = NA, include_srid = NA) {
  cpp_wkt_translate_wksxp(
    wkt,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid
  )
}

#' @rdname wkb_translate_wkt
#' @export
wksxp_translate_wkt <- function(wksxp, include_z = NA, include_m = NA, include_srid = NA,
                                 precision = 16, trim = TRUE) {
  cpp_wksxp_translate_wkt(
    wksxp,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    precision = precision,
    trim = trim
  )
}

#' @rdname wkb_translate_wkt
#' @export
wksxp_translate_wkb <- function(wksxp, include_z = NA, include_m = NA, include_srid = NA,
                                endian = wk_platform_endian(), buffer_size = 2048) {
  cpp_wksxp_translate_wkb(
    wksxp,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid,
    endian = endian,
    bufferSize = buffer_size
  )
}

#' @rdname wkb_translate_wkt
#' @export
wksxp_translate_wksxp <- function(wksxp, include_z = NA, include_m = NA, include_srid = NA) {
  cpp_wksxp_translate_wksxp(
    wksxp,
    includeZ = include_z,
    includeM = include_m,
    includeSRID = include_srid
  )
}

#' @rdname wkb_translate_wkt
#' @export
wk_platform_endian <- function() {
  match(.Platform$endian, c("big", "little")) - 1L
}
