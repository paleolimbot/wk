
#' Compute overview grid specifications
#'
#' @inheritParams grd_summary
#' @param level An integer describing the overview level. This is related to
#'   the `step` value by a power of 2 (i.e., a level of `1` indicates a step of
#'   `2`, a level of `2` indicates a step of `4`, etc.).
#' @param levels A vector of `level` values or `NULL` to use a sequence from
#'   0 to the level that would result in a 1 x 1 grid.
#'
#' @return A [grd()]
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' grd_overview_summary(grid)
#'
grd_overview_summary <- function(grid, levels = NULL) {
  if (is.null(levels)) {
    s <- grd_summary(grid)
    level0 <- max(floor(log2(c(s$nx, s$ny)))) + 1L
    levels <- 0:level0
  }

  overviews <- lapply(levels, function(level) grd_overview_template(grid, level))
  summaries <- lapply(overviews, grd_summary)
  summary_df <- lapply(summaries, new_data_frame)
  cbind(level = levels, do.call(rbind, summary_df))
}

grd_overview_template <- function(grid, level) {
  if (length(level) == 1L) {
    level <- c(level, level)
  }

  s <- grd_summary(grid)
  step <- 2 ^ level
  step_clamp <- pmax(1L, pmin(c(s$ny, s$nx), step))
  level_clamp <- ceiling(log2(step_clamp))
  step_final <- 2 ^ level_clamp

  final_dy <- s$dy * step_final[1]
  final_dx <- s$dx * step_final[2]
  final_ny <- ceiling(s$ny / step_final[1])
  final_nx <- ceiling(s$nx / step_final[2])

  final_bbox <- rct(
    s$xmin,
    s$ymax - final_ny * final_dy,
    s$xmin + final_nx * final_dx,
    s$ymax,
    crs = wk_crs(grid)
  )

  grd_rct(
    array(dim = c(final_ny, final_nx, 0)),
    final_bbox
  )
}

#' Extract normalized grid tiles
#'
#' @inheritParams grd_overview_summary
#' @inheritParams grd_subset
#'
#' @return A [grd_subset()]ed version
#' @export
#'
#' @examples
#' grid <- grd_rct(volcano)
#' plot(grd_tile(grid, 4, 1, 1))
#'
#' plot(grd_tile(grid, 3, 1, 1), add = TRUE)
#' plot(grd_tile(grid, 3, 1, 2), add = TRUE)
#' plot(grd_tile(grid, 3, 2, 1), add = TRUE)
#' plot(grd_tile(grid, 3, 2, 2), add = TRUE)
#'
#' grid <- as_grd_xy(grd_tile(grid, 4, 1, 1))
#' plot(grid, add = T, pch = ".")
#' plot(grd_tile(grid, 3, 1, 1), add = TRUE, col = "green", pch = ".")
#' plot(grd_tile(grid, 3, 1, 2), add = TRUE, col = "red", pch = ".")
#' plot(grd_tile(grid, 3, 2, 1), add = TRUE, col = "blue", pch = ".")
#' plot(grd_tile(grid, 3, 2, 2), add = TRUE, col = "magenta", pch = ".")
#'
grd_tile <- function(grid, level, i, j = NULL) {
  UseMethod("grd_tile")
}

#' @rdname grd_tile
#' @export
grd_tile.grd_rct <- function(grid, level, i, j = NULL) {
  overview <- grd_overview_template(grid, level)
  bbox <- grd_cell_rct(overview, i, j)
  ranges <- grd_cell_range(grid, bbox, snap = list(grd_snap_next, grd_snap_previous))
  grd_subset(grid, ranges)
}

#' @rdname grd_tile
#' @export
grd_tile.grd_xy <- function(grid, level, i, j = NULL) {
  grid_rct <- as_grd_rct(grid)
  overview <- grd_overview_template(grid_rct, level)
  bbox <- grd_cell_rct(overview, i, j)
  ranges <- grd_cell_range(grid, bbox, snap = list(grd_snap_next, grd_snap_previous))
  grd_subset(grid, ranges)
}
