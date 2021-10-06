
#' Resample grid objects
#'
#' @inheritParams grd_summary
#' @param template A grid whose `bbox`, `nrow()`, and `ncol()` will be used
#'   to determine the `bbox`, `nrow()`, and `ncol()` of the output.
#' @inheritParams wk_transform
#' @param ... Passed to S3 methods
#'
#' @return A grid with the same class, `bbox`, `nrow()`, and `ncol()` as
#'   `template`.
#' @export
#'
grd_resample_nearest <- function(grid, template, trans = wk_affine_identity(), ...) {
  UseMethod("grd_resample_nearest")
}
