
wk_transform <- function(handleable, trans, ...) {

}

wk_transform_filter <- function(handler, trans) {

}

wk_transform_matrix <- function(x, trans) {

}

wk_transform_list <- function(x, trans) {

}

wk_trans_inverse <- function(trans, ...) {
  UseMethod("wk_trans_inverse")
}

as_wk_trans <- function(x, ...) {
  UseMethod("as_wk_trans")
}

as_wk_trans.wk_trans <- function(x, ...) {
  x
}

new_wk_trans <- function(trans_ptr, subclass = character()) {
  stopifnot(typeof(trans_ptr) == "externalptr")
  structure(trans_ptr, class = union(subclass, "wk_trans"))
}
