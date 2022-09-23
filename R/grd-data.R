
#' Grid data interface
#'
#' @inheritParams grd_summary
#' @param grid_data The `data` member of a [grd()]. This is typically an
#'   array but can also be an S3 object with the proper methods
#'   implemented (e.g., [grd_data_generic()]).
#' @param ptype The R object type that should be used to represent
#'   the data.
#' @param template A [grd()] or [grd_data()] whose x-y dimensions will be
#'   used as a template to allocate a new [grd_data()].
#' @param i,j 1-based index values. `i` indices correspond to decreasing
#'   `y` values; `j` indices correspond to increasing `x` values.
#'   Values outside the range `1:nrow|ncol(data)` will be censored to
#'   `NA` including 0 and negative values.
#' @param value A vector of values to assign
#' @param ... Passed to S3 methods.
#'
#' @return
#'   - `grd_data()` returns the data member of a [grd()].
#'   - `grd_data_subset()` returns a subset of the data independent of the
#'     parent [grd()] but using the same indexing rules as [grd_subset()].
#'     The non-xy dimensions of `grid_data` are not affected.
#'   - `grd_data_order()` returns `c("y", "x")` for
#'     data with a column-major internal ordering and
#'     `c("x", "y")` for data with a row-major internal
#'     ordering. Both 'x' and 'y' can be modified with
#'     a negative sign to indicate right-to-left
#'     or bottom-to-top internal ordering, respectively. This value
#'     does not affect the axis order or axis direction used to index
#'     in [grd_subset()] or [grd_data_subset()].
#' @export
#'
#' @examples
#' grd_data(grd(nx = 3, ny = 2))
#' grd_data_subset(matrix(1:6, nrow = 2), 2, 3)
#'
grd_data <- function(grid) {
  grid$data
}

#' @rdname grd_data
#' @export
grd_data_ptype <- function(grid_data) {
  UseMethod("grd_data_ptype")
}

#' @rdname grd_data
#' @export
grd_data_collect <- function(grid_data, i = NULL, j = NULL, ...,
                             ptype = grd_data_ptype(grid_data)) {
  UseMethod("grd_data_collect")
}

#' @rdname grd_data
#' @export
grd_data_subset <- function(grid_data, i = NULL, j = NULL, ...) {
  UseMethod("grd_data_subset")
}

#' @rdname grd_data
#' @export
grd_data_subset_assign <- function(grid_data, i = NULL, j = NULL, ..., value) {
  UseMethod("grd_data_subset_assign")
}

#' @rdname grd_data
#' @export
grd_data_from_template <- function(grid_data, template) {
  UseMethod("grd_data_from_template")
}

#' @rdname grd_data
#' @export
grd_data_order <- function(grid_data) {
  UseMethod("grd_data_order")
}

#' @rdname grd_data
#' @export
grd_data_ptype.array <- function(grid_data) {
  grid_data[integer(0)]
}

#' @rdname grd_data
#' @export
grd_data_collect.array <- function(grid_data, i = NULL, j = NULL, ...,
                                   ptype = grd_data_ptype(grid_data)) {
  grid_data <- grd_data_subset(grid_data, i = i, j = j, ...)

  if (identical(ptype, grd_data_ptype(grid_data))) {
    # don't need to modify grid_data
  } else if (inherits(ptype, "logical")) {
    storage.mode(grid_data) <- "logical"
  } else if (inherits(ptype, "integer")) {
    storage.mode(grid_data) <- "integer"
  } else if (inherits(ptype, "numeric")) {
    storage.mode(grid_data) <- "double"
  } else if (inherits(ptype, "raw")) {
    storage.mode(grid_data) <- "raw"
  } else if (inherits(ptype, "character")) {
    storage.mode(grid_data) <- "character"
  } else {
    stop(
      paste0("Can't convert grid data to '", class(grid_data)[1], "'"),
      call. = FALSE
    )
  }

  grid_data
}

#' @rdname grd_data
#' @export
grd_data_subset.default <- function(grid_data, i = NULL, j = NULL, ...) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  # we want to keep everything for existing dimensions
  # this means generating a list of missings to fill
  # the correct number of additional dimensions
  n_more_dims <- length(dim(grid_data)) - 2L
  more_dims <- alist(1, )[rep(2, n_more_dims)]
  do.call("[", c(list(grid_data, ij$i, ij$j), more_dims, list(drop = FALSE)))
}

#' @rdname grd_data
#' @export
grd_data_subset_assign.default <- function(grid_data, i = NULL, j = NULL, ..., value) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  # we want to keep everything for existing dimensions
  # this means generating a list of missings to fill
  # the correct number of additional dimensions
  n_more_dims <- length(dim(grid_data)) - 2L
  more_dims <- alist(1, )[rep(2, n_more_dims)]
  do.call("[<-", c(list(grid_data, ij$i, ij$j), more_dims, list(value = value)))
}

#' @rdname grd_data
#' @export
grd_data_subset.nativeRaster <- function(grid_data, i = NULL, j = NULL, ...) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  attrs <- attributes(grid_data)
  dim(grid_data) <- rev(dim(grid_data))
  grid_data <- grid_data[ij$j, ij$i, drop = FALSE]
  attrs$dim <- rev(dim(grid_data))
  attributes(grid_data) <- attrs
  grid_data
}

#' @rdname grd_data
#' @export
grd_data_subset_assign.nativeRaster <- function(grid_data, i = NULL, j = NULL, ..., value) {
  ij <- ij_from_args(i, j)
  ij$i <- ij_expand_one(ij$i, dim(grid_data)[1], out_of_bounds = "censor")
  ij$j <- ij_expand_one(ij$j, dim(grid_data)[2], out_of_bounds = "censor")

  # special case the nativeRaster, whose dims are lying about
  # the ordering needed to index it
  attrs <- attributes(grid_data)
  dim(grid_data) <- rev(dim(grid_data))
  grid_data <- grid_data[ij$j, ij$i] <- value
  attrs$dim <- rev(dim(grid_data))
  attributes(grid_data) <- attrs
  grid_data
}

#' @rdname grd_data
#' @export
grd_data_from_template.default <- function(grid_data, template) {
  # template controls dims in the x-y domain; but grid_data
  # controls dims otherwise
  new_dims <- dim(grid_data)
  new_dims[1:2] <- dim(template)[1:2]
  ptype <- grd_data_ptype(grid_data)
  if (length(new_dims) == 2) {
    matrix(ptype[NA_integer_], nrow = new_dims[1], ncol = new_dims[2])
  } else {
    array(ptype[NA_integer_], dim = new_dims)
  }
}

#' @rdname grd_data
#' @export
grd_data_order.default <- function(grid_data) {
  attr(grid_data, "grd_data_order") %||%
    c("y", "x", rep(NA_character_, length(dim(grid_data)) - 2L))
}

#' @rdname grd_data
#' @export
grd_data_order.nativeRaster <- function(grid_data) {
  c("x", "y")
}
