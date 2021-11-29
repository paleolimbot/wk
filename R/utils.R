
`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}

new_data_frame <- function(x) {
  structure(x, row.names = c(NA, length(x[[1]])), class = "data.frame")
}

# rep_len became an S3 generic in R 3.6, so we need to use
# something else to make sure recycle_common() works on old
# R versions
rep_len_compat <- function(x, length_out) {
  rep(x, length.out = length_out)
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

  dots[lengths != final_length] <- lapply(dots[lengths != final_length], rep_len_compat, final_length)
  dots
}

is_vector_class <- function(x) {
  identical(class(x[integer(0)]), class(x))
}
