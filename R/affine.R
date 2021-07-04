
#' Affine transformer
#'
#' @param trans_matrix A 3x3 transformation matrix
#' @param x A [wk_trans_affine()]
#' @param dx,dy Coordinate offsets in the x and y direction
#' @param scale_x,scale_y Scale factor to apply in the x and y directions, respectively
#' @param rct_in,rct_out The input and output bounds
#' @param rotation_deg A rotation to apply in degrees counterclockwise.
#' @param src,dst Point vectors of control points used to estimate the affine mapping
#'   (using [base::qr.solve()]).
#' @param ... Zero or more transforms in the order they should be applied.
#'
#' @export
#'
wk_trans_affine <- function(trans_matrix) {
  new_wk_trans(.Call(wk_c_trans_affine_new, trans_matrix), "wk_trans_affine")
}

#' @export
wk_trans_inverse.wk_trans_affine <- function(trans, ...) {
  wk_affine_invert(trans)
}

#' @rdname wk_trans_affine
#' @export
wk_affine_identity <- function() {
  wk_affine_translate(0, 0)
}

#' @rdname wk_trans_affine
#' @export
wk_affine_rotate <- function(rotation_deg) {
  theta <- -rotation_deg * pi / 180
  trans_matrix <- matrix(
    c(
      cos(theta), +sin(theta), 0,
      -sin(theta), cos(theta), 0,
      0, 0, 1
    ),
    nrow = 3,
    byrow = TRUE
  )

  wk_trans_affine(trans_matrix)
}

#' @rdname wk_trans_affine
#' @export
wk_affine_scale <- function(scale_x = 1, scale_y = 1) {
  wk_trans_affine(matrix(c(scale_x, 0, 0, 0, scale_y, 0, 0, 0, 1), ncol = 3))
}

#' @rdname wk_trans_affine
#' @export
wk_affine_translate <- function(dx = 0, dy = 0) {
  wk_trans_affine(matrix(c(1, 0, 0, 0, 1, 0, dx, dy, 1), ncol = 3))
}

#' @rdname wk_trans_affine
#' @export
wk_affine_fit <- function(src, dst) {
  src <- as_xy(src)
  dst <- as_xy(dst)
  n <- length(src)
  stopifnot(length(src) == length(dst))
  src <- unclass(src)
  dst <- unclass(dst)

  src_mat <- cbind(src$x, src$y, rep_len(1, n))
  dst_mat <- cbind(dst$x, dst$y, rep_len(1, n))

  wk_trans_affine(t(qr.solve(src_mat, dst_mat)))
}

#' @rdname wk_trans_affine
#' @export
wk_affine_rescale <- function(rct_in, rct_out) {
  # use bbox to sanitize input as rct of length 1
  rct_in <- unclass(wk_bbox(rct_in))
  rct_out <- unclass(wk_bbox(rct_out))

  width_in <- rct_in$xmax - rct_in$xmin
  height_in <- rct_in$ymax - rct_in$ymin
  width_out <- rct_out$xmax - rct_out$xmin
  height_out <- rct_out$ymax - rct_out$ymin

  dx <- rct_out$xmin - rct_in$xmin
  dy <- rct_out$ymin - rct_in$ymin

  wk_affine_compose(
    wk_affine_translate(dx, dy),
    wk_affine_scale(width_out / width_in, height_out / height_in)
  )
}

#' @rdname wk_trans_affine
#' @export
wk_affine_compose <- function(...) {
  trans_matrix <- Reduce(
    `%*%`,
    lapply(rev(list(...)), as.matrix),
    init = as.matrix(wk_affine_identity())
  )
  wk_trans_affine(trans_matrix)
}

#' @rdname wk_trans_affine
#' @export
wk_affine_invert <- function(x) {
  wk_trans_affine(solve(as.matrix(x)))
}

#' @export
as.matrix.wk_trans_affine <- function(x, ...) {
  .Call(wk_c_trans_affine_as_matrix, x)
}

#' @export
format.wk_trans_affine <- function(x, ...) {
  format(as.matrix(x), ...)
}

#' @export
print.wk_trans_affine <- function(x, ...) {
  cat("<wk_trans_affine>\n")
  print(as.matrix(x), ...)
  invisible(x)
}
