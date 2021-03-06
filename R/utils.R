
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}

recycle_common <- function(...) {
  dots <- list(...)
  lengths <- vapply(dots, length, integer(1))
  non_constant_lengths <- unique(lengths[lengths != 1])
  if (length(non_constant_lengths) == 0) {
    final_length <- 1
  } else if(length(non_constant_lengths) == 1) {
    final_length <- non_constant_lengths
  } else {
    lengths_label <- paste0(non_constant_lengths, collapse = ", ")
    stop(sprintf("Incompatible lengths: %s", lengths_label))
  }

  lapply(dots, rep_len, final_length)
}
