
#' Extract normalized grid tiles
#'
#' @inheritParams grd_overview
#' @inheritParams grd_subset
#'
#' @return A [grd_subset()]ed version
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' plot(grd_tile(grid, 4, 1, 1))
#' plot(grd_tile(grid, 3, 1, 1), add = TRUE)
#'
grd_tile <- function(grid, level, i, j = NULL) {
  UseMethod("grd_tile")
}

#' @rdname grd_tile
#' @export
grd_tile.grd_rct <- function(grid, level, i, j = NULL) {
  overview <- grd_overview(grid, level)
  bbox <- grd_cell_rct(overview, i, j)
  ranges <- grd_cell_range(grid, bbox, snap = list(grd_snap_next, grd_snap_previous))
  grd_subset(grid, ranges)
}
