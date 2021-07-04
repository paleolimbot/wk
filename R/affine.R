
#' Affine transformer
#'
#' @param trans_matrix A 3x3 transformation matrix
#' @param x A [wk_trans_affine()]
#' @param dx,dy Coordinate offsets in the x and y direction
#' @param rct_in,rct_out The input and output bounds
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
  theta <- rotation_deg * pi / 180
  matrix(
    c(
      cos(theta), +sin(theta), 0,
      -sin(theta), cos(theta), 0,
      0, 0, 1
    ),
    nrow = 3,
    byrow = TRUE
  )
}

#' @rdname wk_trans_affine
#' @export
wk_affine_translate <- function(dx = 0, dy = 0) {
  wk_trans_affine(matrix(c(1, 0, 0, 0, 1, 0, dx, dy, 1), ncol = 3))
}

#' @rdname wk_trans_affine
#' @export
wk_affine_rescale <- function(rct_in, rct_out) {

}

#' @rdname wk_trans_affine
#' @export
wk_affine_compose <- function(...) {
  trans_matrix <- Reduce(`%*%`, lapply(list(...), as.matrix))
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
