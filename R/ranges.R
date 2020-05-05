
#' Extract ranges information
#'
#' @inheritParams wkb_translate_wkt
#' @param na.rm Pass `TRUE` to not consider missing (nan) values
#' @param finite Pass `TRUE` to only consider finite
#'   (non-missing, non-infinite) values.
#'
#' @return A data.frame with columns:
#' - `xmin`, `ymin`, `zmin`, and `mmin`: Minimum coordinate values
#' - `xmax`, `ymax`, `zmax`, and `mmax`: Maximum coordinate values
#'
#' @examples
#' wkt_ranges("POINT (30 10)")
#'
wkb_ranges <- function(wkb, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_ranges_wkb(wkb, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}

#' @rdname wkb_ranges
#' @export
wkt_ranges <- function(wkt, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_ranges_wkt(wkt, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}

#' @rdname wkb_ranges
#' @export
wksxp_ranges <- function(wksxp, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_ranges_wksxp(wksxp, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}

#' @rdname wkb_ranges
#' @export
wkb_feature_ranges <- function(wkb, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_feature_ranges_wkb(wkb, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}

#' @rdname wkb_ranges
#' @export
wkt_feature_ranges <- function(wkt, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_feature_ranges_wkt(wkt, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}

#' @rdname wkb_ranges
#' @export
wksxp_feature_ranges <- function(wksxp, na.rm = FALSE, finite = FALSE) {
  ranges <- cpp_feature_ranges_wksxp(wksxp, naRm = na.rm, onlyFinite = finite)
  # slightly faster than data.frame()
  structure(ranges, row.names = seq_len(length(ranges[[1]])), class = "data.frame")
}
