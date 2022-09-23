
#' Wrap data sources with non-standard ordering
#'
#' Whereas [grd()] objects always index [grd_data()] using
#' y, x axis order with y values decreasing with increasing
#' index value, data are often stored in other configurations.
#' The [grd_data_generic()] class wraps the common case where
#' axis order or axis direction differs from the default.
#' The resulting object retains a reference to the underlying
#' data but takes care of rearranging the calls to `dim()`,
#' `[`, and `[<-` such that the object can be indexed
#' and modified in the same way.
#'
#' @inheritParams grd_data
#' @param data_order A character vector with the
#'   same length as `dim(grid_data)` specifying the
#'   axis order and axis direction of indices in the
#'   x y direction. The default `c("y", "x")` indicates
#'   column-major ordering with y values decreasing
#'   in the positive i index direction and x values increasing
#'   in the positive j index direction. Use `"-y"` or `"-x"` to
#'   switch axis directions. Use `NA` to indicate a non-xy
#'   dimension.
#'
#' @return An object of class `grd_data_generic`.
#' @export
#'
grd_data_generic <- function(grid_data,
                             data_order = grd_data_order(grid_data),
                             ptype = grd_data_ptype(grid_data)) {
  stopifnot(
    length(data_order) == length(dim(grid_data))
  )

  structure(
    list(
      grid_data = grid_data,
      data_order = data_order,
      ptype = ptype
    ),
    class = "grd_data_generic"
  )
}

#' @export
dim.grd_data_generic <- function(x) {
  axis_order <- gsub("^-", "", x$data_order)
  xy <- c(which(axis_order == "y"), which(axis_order == "x"))
  dims <- dim(x$grid_data)
  dims[c(xy, setdiff(seq_along(dims), xy))]
}

#' @export
grd_data_ptype.grd_data_generic <- function(grid_data) {
  grid_data$ptype
}

#' @export
grd_data_order.grd_data_generic <- function(grid_data) {
  grid_data$data_order
}

#' @export
grd_data_collect.grd_data_generic <- function(grid_data, i = NULL, j = NULL, ...,
                                              ptype = grd_data_ptype(grid_data)) {
  # calculate the subset first
  grid_data <- grd_data_subset(grid_data, i = i, j = j, ...)
  dims <- dim(grid_data)
  axis_order <- gsub("^-", "", grid_data$data_order)
  xy <- c(which(axis_order == "y"), which(axis_order == "x"))

  # potentially call aperm() to get axes in the correct order
  perm <- seq_along(dims)
  perm <- perm[c(xy, setdiff(seq_along(perm), xy))]
  if (!all(perm == seq_along(dims))) {
    data <- aperm(grid_data$grid_data, perm)
  } else {
    data <- grid_data$grid_data
  }

  # generate index vectors
  xy_rev <- grepl("^-", grid_data$data_order[!is.na(grid_data$data_order)])

  if (xy_rev[2]) {
    subset_i <- rev(seq_len(dims[1]))
  } else {
    subset_i <- seq_len(dims[1])
  }

  if (xy_rev[1]) {
    subset_j <- rev(seq_len(dims[2]))
  } else {
    subset_j <- seq_len(dims[2])
  }

  data_subset_args <- c(
    list(data, subset_i, subset_j),
    rep(list(quote(expr = )), length(dims) - 2L),
    list(drop = FALSE)
  )

  data <- do.call("[", data_subset_args)
  grd_data_collect(data, ptype = ptype)
}

#' @export
`[.grd_data_generic` <- function(x, ..., drop = FALSE) {
  # doesn't make sense with drop argument
  stopifnot(identical(drop, FALSE), ...length() == length(dim(x)))

  # requires some magic to rearrange arguments that may be missing
  call <- match.call()
  call[[1]] <- quote(`[`)
  call[[2]] <- x$grid_data
  call$drop <- FALSE

  axis_order <- gsub("^-", "", x$data_order)
  xy <- c(which(axis_order == "y"), which(axis_order == "x"))

  # apply index reverse if needed
  xy_rev <- grepl("^-", x$data_order[!is.na(x$data_order)])
  dims <- dim(x$grid_data)

  if (!identical(call[[2L + xy[1]]], quote(expr = ))) {
    if (xy_rev[1]) {
      i <- rev(dims[xy[1]] - ...elt(xy[1]) + 1L)
      i[i <= 0 | i > dims[xy[1]]] <- NA_integer_
      call[[2L + xy[1]]] <- i
    } else {
      call[[2L + xy[1]]] <- ...elt(xy[1])
    }
  }

  if (!identical(call[[2L + xy[2]]], quote(expr = ))) {
    if (xy_rev[2]) {
      j <- rev(dims[xy[2]] - ...elt(xy[2]) + 1L)
      j[j <= 0 | j > dims[xy[2]]] <- NA_integer_
      call[[2L + xy[2]]] <- j
    } else {
      call[[2L + xy[2]]] <- ...elt(xy[2])
    }
  }

  call[seq_len(...length()) + 2L] <- call[c(xy, setdiff(seq_along(dims), xy)) + 2L]
  x$grid_data <- eval(call, envir = parent.frame())
  x
}

#' @export
`[<-.grd_data_generic` <- function(x, ..., value) {
  stopifnot(...length() == length(dim(x)))

  # requires some magic to rearrange arguments that may be missing
  call <- match.call()
  call[[1]] <- quote(`[<-`)
  call[[2]] <- x$grid_data

  axis_order <- gsub("^-", "", x$data_order)
  xy <- c(which(axis_order == "y"), which(axis_order == "x"))

  # resolve `value` to a known axis ordering if it has axes
  if (!is.null(dim(value))) {
    value <- grd_data_collect(value, ptype = x$ptype)

    # potentially call aperm() to get axes in the correct order
    value_dims <- dim(value)
    perm <- seq_along(value_dims)
    perm <- perm[c(xy, setdiff(seq_along(perm), xy))]
    if (!all(perm == seq_along(value_dims))) {
      value <- aperm(value, perm)
    }

    call$value <- value
  }

  # apply axis/index reverse if needed
  xy_rev <- grepl("^-", x$data_order[!is.na(x$data_order)])
  dims <- dim(x$grid_data)

  if (!identical(call[[2L + xy[1]]], quote(expr = ))) {
    if (xy_rev[1]) {
      i <- dims[xy[1]] - ...elt(xy[1]) + 1L
      i[i <= 0 | i > dims[xy[1]]] <- NA_integer_
      call[[2L + xy[1]]] <- i
    } else {
      call[[2L + xy[1]]] <- ...elt(xy[1])
    }
  }

  if (!identical(call[[2L + xy[2]]], quote(expr = ))) {
    if (xy_rev[2]) {
      j <- dims[xy[2]] - ...elt(xy[2]) + 1L
      j[j <= 0 | j > dims[xy[2]]] <- NA_integer_
      call[[2L + xy[2]]] <- j
    } else {
      call[[2L + xy[2]]] <- ...elt(xy[2])
    }
  }

  call[seq_len(...length()) + 2L] <- call[c(xy, setdiff(seq_along(dims), xy)) + 2L]
  x$grid_data <- eval(call, envir = parent.frame())
  x
}
