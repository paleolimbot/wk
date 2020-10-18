
#' Efficient point vectors
#'
#' @param x,y,z,m Coordinate values.
#' @param dims A set containing one or more of `c("x", "y", "z", "m")`.
#' @param ... Passed to methods.
#'
#' @return A vector of coordinate values.
#' @export
#'
#' @examples
#' xy(1:5, 1:5)
#' xyz(1:5, 1:5, 10)
#' xym(1:5, 1:5, 10)
#' xyzm(1:5, 1:5, 10, 12)
#'
xy <- function(x = double(), y = double()) {
  vec <- new_wk_xy(recycle_common(x = as.double(x), y = as.double(y)))
  validate_wk_xy(vec)
  vec
}

#' @rdname xy
#' @export
xyz <- function(x = double(), y = double(), z = double()) {
  vec <- new_wk_xyz(recycle_common(x = as.double(x), y = as.double(y), z = as.double(z)))
  validate_wk_xyz(vec)
  vec
}

#' @rdname xy
#' @export
xym <- function(x = double(), y = double(), m = double()) {
  vec <- new_wk_xym(recycle_common(x = as.double(x), y = as.double(y), m = as.double(m)))
  validate_wk_xym(vec)
  vec
}

#' @rdname xy
#' @export
xyzm <- function(x = double(), y = double(), z = double(), m = double()) {
  vec <- new_wk_xyzm(
    recycle_common(
      x = as.double(x),
      y = as.double(y),
      z = as.double(z),
      m = as.double(m)
    )
  )
  validate_wk_xyzm(vec)
  vec
}

#' @rdname xy
#' @export
xy_dims <- function(x) {
  names(unclass(x))
}

#' @rdname xy
#' @export
as_xy <- function(x, ...) {
  UseMethod("as_xy")
}

#' @rdname xy
#' @export
as_xy.wk_xy <- function(x, ..., dims = NULL) {
  if (is.null(dims)) {
    x
  } else if (setequal(dims, c("x", "y"))) {
    new_wk_xy(fill_missing_dims(unclass(x), c("x", "y"), length(x)))

  } else if (setequal(dims, c("x", "y", "z"))) {
    new_wk_xyz(fill_missing_dims(unclass(x), c("x", "y", "z"), length(x)))

  } else if (setequal(dims, c("x", "y", "m"))) {
    new_wk_xym(fill_missing_dims(unclass(x), c("x", "y", "m"), length(x)))

  } else if (setequal(dims, c("x", "y", "z", "m"))) {
    new_wk_xyzm(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), length(x)))

  } else {
    stop("Unknown dims in as_xy().", call. = FALSE)
  }
}

#' @rdname xy
#' @export
as_xy.matrix <- function(x, ...) {
  x[] <- as.numeric(x)
  colnames(x) <- tolower(colnames(x))
  cols <- colnames(x)

  if (!is.null(cols)) {
    dim_cols <- intersect(c("x", "y", "z", "m"), cols)
    if (length(dim_cols) == 0) {
      stop(
        paste0(
          "Can't guess dimensions of matrix with column names\n",
          paste0("'", cols, "'", collapse = ", ")
        ),
        call. = FALSE
      )
    }

    if (!identical(dim_cols, colnames(x))) {
      x <- x[, dim_cols, drop = FALSE]
    }
  }

  # prevent named subsets
  dimnames(x) <- NULL

  if (ncol(x) == 2) {
    new_wk_xy(
      list(
        x = x[, 1, drop = TRUE],
        y = x[, 2, drop = TRUE]
      )
    )
  } else if (ncol(x) == 4) {
    new_wk_xyzm(
      list(
        x = x[, 1, drop = TRUE],
        y = x[, 2, drop = TRUE],
        z = x[, 3, drop = TRUE],
        m = x[, 4, drop = TRUE]
      )
    )
  } else if (identical(cols, c("x", "y", "m"))) {
    new_wk_xym(
      list(
        x = x[, 1, drop = TRUE],
        y = x[, 2, drop = TRUE],
        m = x[, 3, drop = TRUE]
      )
    )
  } else if (ncol(x) == 3) {
    new_wk_xyz(
      list(
        x = x[, 1, drop = TRUE],
        y = x[, 2, drop = TRUE],
        z = x[, 3, drop = TRUE]
      )
    )
  } else {
    stop(
      sprintf("Can't guess dimensions of matrix with %s columns", ncol(x)),
      call. = FALSE
    )
  }
}

#' @rdname xy
#' @export
as_xy.data.frame <- function(x, ..., dims = NULL) {
  if (is.null(dims)) {
    dims <- intersect(c("x", "y", "z", "m"), names(x))
  }

  if (setequal(dims, c("x", "y"))) {
    new_wk_xy(fill_missing_dims(unclass(x), c("x", "y"), nrow(x)))

  } else if (setequal(dims, c("x", "y", "z"))) {
    new_wk_xyz(fill_missing_dims(unclass(x), c("x", "y", "z"), nrow(x)))

  } else if (setequal(dims, c("x", "y", "m"))) {
    new_wk_xym(fill_missing_dims(unclass(x), c("x", "y", "m"), nrow(x)))

  } else if (setequal(dims, c("x", "y", "z", "m"))) {
    new_wk_xyzm(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), nrow(x)))

  } else {
    stop("Unknown dims in as_xy.data.frame().", call. = FALSE)
  }
}

fill_missing_dims <- function(x, dims, len) {
  missing_dims <- setdiff(dims, names(x))
  x[missing_dims] <- lapply(
    stats::setNames(missing_dims, missing_dims),
    function(x) rep_len(NA_real_, len)
  )
  lapply(x[dims], as.double)
}

#' @export
as_xy.wk_wkt <- function(x, ..., dims = NULL) {
  as_xy_wkx(x, translator = wkt_translate_xyzm, dims = dims)
}

#' @export
as_xy.wk_wkb <- function(x, ..., dims = NULL) {
  as_xy_wkx(x, translator = wkb_translate_xyzm, dims = dims)
}

#' @export
as_xy.wk_wksxp <- function(x, ..., dims = NULL) {
  as_xy_wkx(x, translator = wksxp_translate_xyzm, dims = dims)
}

as_xy_wkx <- function(x, translator, dims) {
  if (is.null(dims)) {
    result <- translator(x, include_z = NA, include_m = NA)
    dims <- names(result)
  } else {
    result <- translator(x, include_z = TRUE, include_m = TRUE)
    dims <- intersect(c("x", "y", "z", "m"), dims)
  }

  if (identical(dims, c("x", "y"))) {
    new_wk_xy(result[dims])

  } else if (identical(dims, c("x", "y", "z"))) {
    new_wk_xyz(result[dims])

  } else if (identical(dims, c("x", "y", "m"))) {
    new_wk_xym(result[dims])

  } else if (identical(dims, c("x", "y", "z", "m"))) {
    new_wk_xyzm(result[dims])

  } else {
    stop("Unknown dimensions.", call. = FALSE)
  }
}


#' S3 details for xy objects
#'
#' @param x A [xy()] object.
#'
#' @export
#'
new_wk_xy <- function(x = list(x = double(), y = double())) {
  structure(x, class = c("wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xyz <- function(x = list(x = double(), y = double(), z = double())) {
  structure(x, class = c("wk_xyz", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xym <- function(x = list(x = double(), y = double(), m = double())) {
  structure(x, class = c("wk_xym", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
new_wk_xyzm <- function(x = list(x = double(), y = double(), z = double(), m = double())) {
  structure(x, class = c("wk_xyzm", "wk_xyz", "wk_xym", "wk_xy", "wk_rcrd"))
}

#' @rdname new_wk_xy
#' @export
validate_wk_xy <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xyz <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "z")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xym <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "m")))
  invisible(x)
}

#' @rdname new_wk_xy
#' @export
validate_wk_xyzm <- function(x) {
  validate_wk_rcrd(x)
  stopifnot(identical(names(unclass(x)), c("x", "y", "z", "m")))
  invisible(x)
}

#' @export
as_wkt.wk_xy <- function(x, ...) {
  new_wk_wkt(xyzm_translate_wkt(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), length(x))))
}

#' @export
as_wkb.wk_xy <- function(x, ...) {
  new_wk_wkb(xyzm_translate_wkb(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), length(x))))
}

#' @export
as_wksxp.wk_xy <- function(x, ...) {
  new_wk_wksxp(xyzm_translate_wksxp(fill_missing_dims(unclass(x), c("x", "y", "z", "m"), length(x))))
}

#' @export
format.wk_xy <- function(x, ...) {
  x <- unclass(x)
  sprintf("(%s %s)", format(x$x, ...), format(x$y, ...))
}

#' @export
format.wk_xyz <- function(x, ...) {
  x <- unclass(x)
  sprintf("Z (%s %s %s)", format(x$x, ...), format(x$y, ...), format(x$z, ...))
}

#' @export
format.wk_xym <- function(x, ...) {
  x <- unclass(x)
  sprintf("M (%s %s %s)", format(x$x, ...), format(x$y, ...), format(x$m, ...))
}

#' @export
format.wk_xyzm <- function(x, ...) {
  x <- unclass(x)
  sprintf("ZM (%s %s %s %s)", format(x$x, ...), format(x$y, ...), format(x$z, ...), format(x$m, ...))
}

#' @export
`[<-.wk_xy` <- function(x, i, value) {
  replacement <- as_xy(value)
  result <- Map(
    "[<-",
    unclass(x),
    i,
    fill_missing_dims(unclass(replacement), xy_dims(x), length(replacement))
  )

  names(result) <- names(unclass(x))
  structure(result, class = class(x))
}
