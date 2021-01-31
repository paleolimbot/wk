
#' @rdname wk_handle
#' @export
wk_handle.data.frame <- function(handleable, handler, ..., .env = parent.frame()) {
  col <- handleable_column_name(handleable)
  wk_handle(handleable[[col]], handler, ...)
}

handleable_column_name <- function(df, .env = parent.frame()) {
  has_handle_method <- vapply(df, has_wk_handle_method, FUN.VALUE = logical(1), .env = .env)

  if (sum(has_handle_method) != 1) {
    stop(
      "To be used with wk_handle(), a data.frame must have exactly one handleable column.",
      call. = FALSE
    )
  }

  names(df)[has_handle_method]
}

has_wk_handle_method <- function(x, .env = parent.frame()) {
  !is.null(getS3method("wk_handle", class(x), optional = TRUE, envir = .env))
}
