
#' Use data.frame with wk
#'
#' @inheritParams wk_handle
#' @inheritParams wk_translate
#' @inheritParams wk_crs
#' @inheritParams wk_identity
#' @param .env Passed to [getS3method()], which is used to find
#'   the column in a [data.frame()] for which a [wk_handle()]
#'   method is defined.
#'
#' @export
#'
#' @examples
#' wk_handle(data.frame(a = wkt("POINT (0 1)")), wkb_writer())
#' wk_translate(wkt("POINT (0 1)"), data.frame(col_name = wkb()))
#' wk_translate(data.frame(a = wkt("POINT (0 1)")), data.frame(wkb()))
#'
wk_handle.data.frame <- function(handleable, handler, ..., .env = parent.frame()) {
  col <- handleable_column_name(handleable)
  wk_handle(handleable[[col]], handler, ...)
}

#' @rdname wk_handle.data.frame
#' @export
wk_writer.data.frame <- function(handleable, ...) {
  col <- handleable_column_name(handleable)
  wk_writer(handleable[[col]], ...)
}

#' @rdname wk_handle.data.frame
#' @export
wk_crs.data.frame <- function(x) {
  col <- handleable_column_name(x)
  wk_crs(x[[col]])
}

#' @rdname wk_handle.data.frame
#' @export
wk_set_crs.data.frame <- function(x, crs) {
  col <- handleable_column_name(x)
  x[[col]] <- wk_set_crs(x[[col]], crs)
  x
}

#' @rdname wk_handle.data.frame
#' @export
wk_restore.data.frame <- function(handleable, result, ...) {
  col <- handleable_column_name(handleable)
  if(nrow(handleable) == length(result)) {
    handleable[[col]] <- result
    handleable
  } else if (nrow(handleable) == 1) {
    handleable <- handleable[rep(1L, length(result)), , drop = FALSE]
    handleable[[col]] <- result
    handleable
  } else {
    stop(
      sprintf(
        "Can't assign result of length %d to data frame with %d rows",
        length(result), nrow(handleable)
      ),
      call. = FALSE
    )
  }
}

#' @rdname wk_handle.data.frame
#' @export
wk_restore.tbl_df <- function(handleable, result, ...) {
  tibble::as_tibble(wk_restore.data.frame(handleable, result, ...))
}

#' @rdname wk_handle.data.frame
#' @export
wk_translate.data.frame <- function(handleable, to, ..., .env = parent.frame()) {
  col <- writable_column_name(to)
  col_value <- wk_translate(handleable, to[[col]], ...)

  if (inherits(handleable, "data.frame")) {
    handleable_col <- handleable_column_name(handleable)
    attributes(handleable) <- list(names = names(handleable))
    handleable[handleable_col] <- list(col_value)
    new_data_frame(handleable)
  } else {
    df_raw <- list(col_value)
    names(df_raw) <- col
    new_data_frame(df_raw)
  }
}

#' @rdname wk_handle.data.frame
#' @export
wk_translate.tbl_df <- function(handleable, to, ..., .env = parent.frame()) {
  tibble::as_tibble(wk_translate.data.frame(handleable, to, ..., .env = .env))
}

handleable_column_name <- function(df, .env = parent.frame()) {
  has_method <- vapply(df, has_wk_handle_method, FUN.VALUE = logical(1), .env = .env)

  if (sum(has_method) != 1) {
    stop(
      "To be used with wk_handle(), a data.frame must have exactly one handleable column.",
      call. = FALSE
    )
  }

  names(df)[has_method]
}

has_wk_handle_method <- function(x, .env = parent.frame()) {
  has_s3_method("wk_handle", x, .env)
}

writable_column_name <- function(df, .env = parent.frame()) {
  has_method <- vapply(df, has_wk_writer_method, FUN.VALUE = logical(1), .env = .env)

  if (sum(has_method) != 1) {
    stop(
      "To be used with wk_translate(), a data.frame must have exactly one wk_writer() column.",
      call. = FALSE
    )
  }

  names(df)[has_method]
}

has_wk_writer_method <- function(x, .env = parent.frame()) {
  has_s3_method("wk_writer", x, .env)
}

has_s3_method <- function(fun, obj, .env) {
  for (cls in class(obj)) {
    if (!is.null(utils::getS3method(fun, cls, optional = TRUE, envir = .env))) {
      return(TRUE)
    }
  }

  FALSE
}
