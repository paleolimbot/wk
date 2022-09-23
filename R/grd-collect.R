
#' Collect a grid in memory
#'
#' @inheritParams grd_subset
#' @inheritParams grd_data
#'
#' @return A [grd_rct()] or [grd_xy()]
#' @export
#'
#' @examples
#' data <- grd_data_generic(volcano, data_order = c("-y", "x"))
#' (grid <- grd_rct(data))
#' grd_collect(grid)
#'
grd_collect <- function(grid, i = NULL, j = NULL, ..., ptype = grd_data_ptype(grd_data(grid))) {
  UseMethod("grd_collect")
}

#' @rdname grd_collect
#' @export
grd_collect.default <- function(grid, i = NULL, j = NULL, ...,
                                ptype = grd_data_ptype(grd_data(grid))) {
  # use grd_subset() to calculate the bbox
  grid_empty <- grid
  grid_empty$data <- array(dim = c(dim(grid)[1:2], 0))
  subs <- grd_subset(grid_empty, i, j)

  grid$data <- grd_data_collect(grid$data, i, j, ..., ptype = ptype)
  grid$bbox <- subs$bbox
  grid
}
