
#' Index snap functions
#'
#' These functions can be used in [grd_cell()] and
#' [grd_cell_range()]. These functions differ in the way
#' they round 0.5: [grd_snap_next()] always rounds up
#' and [grd_snap_previous()] always rounds down. You can
#' also use [floor()] and [ceiling()] as index
#' snap functions.
#'
#' @param x A vector of rescaled but non-integer indices
#'
#' @return A vector of integer indices
#' @export
#'
#' @examples
#' grd_snap_next(seq(0, 2, 0.25))
#' grd_snap_previous(seq(0, 2, 0.25))
#'
grd_snap_next <- function(x) {
  ifelse(((x + 0.5) %% 1L) == 0, ceiling(x), round(x))
}

#' @rdname grd_snap_next
#' @export
grd_snap_previous <- function(x) {
  ifelse(((x + 0.5) %% 1L) == 0, floor(x), round(x))
}
