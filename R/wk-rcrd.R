
new_wk_rcrd <- function(x, template) {
  stopifnot(
    is.list(x),
    is.null(attr(x, "class")),
    !is.null(names(x)),
    all(names(x) != "")
  )

  structure(x, class = unique(class(template)), crs = attr(template, "crs", exact = TRUE))
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
format.wk_rcrd <- function(x, ...) {
  vapply(x, function(item) paste0(format(unclass(item), ...), collapse = "\n"), character(1))
}

#' @export
print.wk_rcrd <- function(x, ...) {
  crs <- wk_crs(x)
  if (is.null(crs)) {
    cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  } else {
    cat(sprintf("<%s[%s] with CRS=%s>\n", class(x)[1], length(x), format(crs)))
  }

  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- format(x)
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
str.wk_rcrd <- function(object, ...) {
  str.wk_vctr(object, ...)
}

#' @export
as.character.wk_rcrd <- function(x, ...) {
  format(x, ...)
}

#' @export
is.na.wk_rcrd <- function(x, ...) {
  is_na <- lapply(unclass(x), is.na)
  Reduce("&", is_na)
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
`$.wk_rcrd` <- function(x, i) {
  stop("`$` is not meaningful for 'wk_rcrd' objects", call. = FALSE)
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

  # check CRS compatibility
  Reduce(wk_crs_output, dots)

  new_wk_vctr(do.call(Map, c(list(c), lapply(dots, unclass))), dots[[1]])
}

# data.frame() will call as.data.frame() with optional = TRUE
#' @export
as.data.frame.wk_rcrd <- function(x, ..., optional = FALSE) {
  if (!optional) {
    new_data_frame(unclass(x))
  } else {
    new_data_frame(list(x))
  }
}

#' @export
as.matrix.wk_rcrd <- function(x, ...) {
  x_bare <- unclass(x)
  matrix(
    do.call(c, x_bare),
    nrow = length(x),
    ncol = length(x_bare),
    byrow = FALSE,
    dimnames = list(NULL, names(x_bare))
  )
}
