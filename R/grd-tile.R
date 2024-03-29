
#' Compute overview grid tile
#'
#' A useful workflow for raster data in a memory bounded environment is to
#' chunk a grid into sections or tiles. These functions compute tiles
#' suitable for such processing. Use [grd_tile_summary()] to generate
#' statistics for `level` values to choose for your application.
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
#' grd_tile_summary(grid)
#' grd_tile_template(grid, 3)
#'
grd_tile_template <- function(grid, level) {
  level <- normalize_level(grid, level)
  grid <- as_grd_rct(grid)

  # clamp the step to a reasonable bound
  s <- grd_summary(grid)
  step <- 2 ^ level
  step_clamp <- pmax(1L, pmin(c(s$ny, s$nx), step))
  level_clamp <- ceiling(log2(step_clamp))
  step_final <- 2 ^ level_clamp

  # calculate the new grid parameters
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

#' @rdname grd_tile_template
#' @export
grd_tile_summary <- function(grid, levels = NULL) {
  if (is.null(levels)) {
    s <- grd_summary(grid)
    level0 <- max(floor(log2(c(s$nx, s$ny)))) + 1L
    levels <- 0:level0
  }

  overviews <- lapply(levels, function(level) grd_tile_template(grid, level))
  summaries <- lapply(overviews, grd_summary)
  summary_df <- lapply(summaries, new_data_frame)
  cbind(level = levels, do.call(rbind, summary_df))
}

#' Extract normalized grid tiles
#'
#' Unlike [grd_tile_template()], which returns a [grd()] whose elements are
#' the boundaries of the specified tiles with no data attached, [grd_tile()]
#' returns the actual tile with the data.
#'
#' @inheritParams grd_tile_summary
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
#' plot(grid, add = TRUE, pch = ".")
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
grd_tile.wk_grd_rct <- function(grid, level, i, j = NULL) {
  overview <- grd_tile_template(grid, level)
  bbox <- grd_cell_rct(overview, i, j)
  ranges <- grd_cell_range(grid, bbox, snap = list(grd_snap_next, grd_snap_previous))
  grd_subset(grid, ranges)
}

#' @rdname grd_tile
#' @export
grd_tile.wk_grd_xy <- function(grid, level, i, j = NULL) {
  grid_rct <- as_grd_rct(grid)
  overview <- grd_tile_template(grid_rct, level)
  bbox <- grd_cell_rct(overview, i, j)
  ranges <- grd_cell_range(grid, bbox, snap = list(grd_snap_next, grd_snap_previous))
  grd_subset(grid, ranges)
}

# This could maybe in the future deal with negative levels based on the
# grd_tile_summary() so that one could work down from coarse->fine
normalize_level <- function(grid, level, s = grd_summary(grid)) {
  if (length(level) == 1L) {
    level <- c(level, level)
  }

  level
}
