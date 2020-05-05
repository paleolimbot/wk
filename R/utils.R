
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

new_data_frame <- function(x) {
  structure(x, row.names = seq_len(length(x[[1]])), class = "data.frame")
}
