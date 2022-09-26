
#' Extract values from a grid
#'
#' Unlike [grd_subset()], which subsets like a matrix, [grd_extract()] returns
#' values.
#'
#' @inheritParams grd_summary
#' @inheritParams grd_subset
#' @inheritParams grd_cell
#' @param i,j Index values as in [grd_subset()] except recycled to a common
#'   size.
#'
#' @return A matrix or vector with two fewer dimensions than the input.
#' @export
#'
grd_extract <- function(grid, i = NULL, j = NULL) {
  grd_data_extract(grid$data, i, j)
}

#' @rdname grd_extract
#' @export
grd_extract_nearest <- function(grid, point, out_of_bounds = c("censor", "squish")) {
  out_of_bounds <- match.arg(out_of_bounds)
  s <- grd_summary(grid)
  ij <- grd_cell(grid, point)
  ij <- ij_handle_out_of_bounds2(ij, list(s$ny, s$nx), out_of_bounds)
  grd_data_extract(grid$data, ij)
}

#' @rdname grd_extract
#' @export
grd_data_extract <- function(grid_data, i = NULL, j = NULL) {
  ij <- ij_from_args(i, j)

  # This doesn't make sense in this context where ij is more like a data.frame
  stopifnot(!is.null(ij$i), !is.null(ij$j))

  ij <- recycle_common(i = ij$i, j = ij$j)

  # Again, the nativeRaster is silently row-major
  if (inherits(grid_data, "nativeRaster")) {
    flat_index <- (ij$i - 1L) * dim(grid_data)[2] + (ij$j - 1L) + 1L
    return(array(grid_data[flat_index]))
  }

  # Only implemented for the first two dimensions right now
  if (length(dim(grid_data)) == 2L ||
      prod(dim(grid_data)) == prod(dim(grid_data)[1:2])) {
    flat_index <- (ij$j - 1L) * dim(grid_data)[1] + (ij$i - 1L) + 1L

    result <- array(grid_data[flat_index])

    # Restore missing dimensions
    dim(result) <- c(length(flat_index), dim(grid_data)[-c(1L, 2L)])
    result
  } else {
    stop("grd_data_extract() not implemented for non-matrix-like data")
  }
}
