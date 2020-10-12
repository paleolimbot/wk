
new_wk_rcrd <- function(x, template) {
  stopifnot(
    is.list(x),
    is.null(attr(x, "class")),
    !is.null(names(x)),
    all(names(x) != "")
  )

  structure(x, class = unique(class(template)))
}

validate_wk_rcrd <- function(x) {
  x_bare <- unclass(x)
  stopifnot(
    typeof(x) == "list",
    !is.null(names(x_bare)),
    all(names(x_bare) != ""),
    all(vapply(x_bare, is.double, logical(1)))
  )

  invisible(x)
}

#' @export
print.wk_rcrd <- function(x, ...) {
  cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- format(x)
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
`[.wk_rcrd` <- function(x, i) {
  new_wk_rcrd(lapply(unclass(x), "[", i), x)
}

#' @export
`[[.wk_rcrd` <- function(x, i) {
  x[i]
}

#' @export
names.wk_rcrd <- function(x) {
  NULL
}

#' @export
`names<-.wk_rcrd` <- function(x, value) {
  if (is.null(value)) {
    x
  } else {
    stop("Names of a 'wk_rcrd' must be NULL.")
  }
}

#' @export
length.wk_rcrd <- function(x) {
  length(unclass(x)[[1]])
}

#' @export
rep.wk_rcrd <- function(x, ...) {
  new_wk_rcrd(lapply(unclass(x), rep, ...), x)
}

#' @method rep_len wk_rcrd
#' @export
rep_len.wk_rcrd <- function(x, ...) {
  new_wk_rcrd(lapply(unclass(x), rep_len, ...), x)
}

#' @export
c.wk_rcrd <- function(...) {
  dots <- list(...)
  classes <- lapply(dots, class)
  first_class <- classes[[1]]
  if (!all(vapply(classes, identical, first_class, FUN.VALUE = logical(1)))) {
    stop("Can't combine 'wk_rcrd' objects that do not have identical classes.", call. = FALSE)
  }

  result <- new_wk_vctr(do.call(Map, c(list(c), lapply(dots, unclass))), dots[[1]])
  validator <- get(
    paste0("validate_", classes[1]),
    mode = "function",
    envir = asNamespace("wk")
  )
  validator(result)
  result
}

#' @export
as.data.frame.wk_rcrd <- function(x, ...) {
  new_data_frame(unclass(x))
}

#' @export
as.matrix.wk_rcrd <- function(x, ...) {
  x_bare <- unclass(x)
  matrix(
    do.call(c, x_bare),
    nrow = length(x),
    ncol = length(x_bare),
    byrow = FALSE,
    dimnames = list(NULL, c("x", "y"))
  )
}
