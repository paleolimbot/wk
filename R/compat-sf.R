
#' @rdname wk_handle
#' @export
wk_handle.sfg <- function(handleable, handler, ...) {
  wk_handle(sf::st_sfc(handleable), handler, ...)
}

#' @rdname wk_handle
#' @export
wk_handle.sf <- function(handleable, handler, ...) {
  wk_handle(sf::st_geometry(handleable), handler, ...)
}

#' @rdname wk_handle
#' @export
wk_handle.bbox <- function(handleable, handler, ...) {
  wk_handle(as_rct(handleable), handler, ...)
}

#' @rdname wk_writer
#' @export
wk_writer.sfc <- function(handleable, ...) {
  sfc_writer()
}

#' @rdname wk_writer
#' @export
wk_writer.sf <- function(handleable, ...) {
  sfc_writer()
}

#' @rdname wk_translate
#' @export
wk_translate.sfc <- function(handleable, to, ...) {
  result <- wk_handle(handleable, sfc_writer(), ...)
  attr(result, "crs") <- sf::st_crs(wk_crs_output(handleable, to))
  result
}

#' @rdname wk_handle.data.frame
#' @export
wk_translate.sf <- function(handleable, to, ...) {
  col_value <- wk_handle(handleable, sfc_writer(), ...)
  crs_out <- sf::st_crs(wk_crs_output(handleable, to))

  if (inherits(handleable, "sf")) {
    sf::st_geometry(handleable) <- col_value
  } else if (inherits(handleable, "data.frame")) {
    col <- handleable_column_name(handleable)
    handleable[col] <- list(col_value)
    handleable <- sf::st_as_sf(handleable, sf_column_name = col)
  } else {
    handleable <- sf::st_as_sf(data.frame(geometry = col_value))
  }

  sf::st_crs(handleable) <- crs_out
  handleable
}

#' @rdname wk_handle.data.frame
#' @export
wk_restore.sf <- function(handleable, result, ...) {
  col <- handleable_column_name(handleable)

  if(nrow(handleable) == length(result)) {
    sf::st_geometry(handleable) <- result
    handleable
  } else if (nrow(handleable) == 1) {
    handleable <- handleable[rep(1L, length(result)), , drop = FALSE]
    sf::st_geometry(handleable) <- result
    handleable
  } else {
    stop(
      sprintf(
        "Can't assign result of length %d to sf with %d rows",
        length(result), nrow(handleable)
      ),
      call. = FALSE
    )
  }
}

#' @export
wk_crs.sfc <- function(x) {
  sf::st_crs(x)
}

#' @export
wk_set_crs.sfc <- function(x, crs) {
  sf::st_crs(x) <- sf::st_crs(crs)
  x
}

#' @export
wk_crs.sf <- function(x) {
  sf::st_crs(x)
}

#' @export
wk_set_crs.sf <- function(x, crs) {
  sf::st_crs(x) <- sf::st_crs(crs)
  x
}

#' @export
wk_crs.sfg <- function(x) {
  sf::NA_crs_
}

# These methods are exported in latest sf, and depending on the order
# a user loads the namespaces, the other method may get called.

#' @export
as_wkb.sfc <- function(x, ...) {
  wk_translate(x, new_wk_wkb(crs = wk_crs_inherit()))
}

#' @export
as_wkb.sfg <- function(x, ...) {
  wk_translate(x, new_wk_wkb(crs = wk_crs_inherit()))
}

#' @export
wk_crs_equal_generic.crs <- function(x, y, ...) {
  x == sf::st_crs(y)
}

wk_crs_from_sf <- function(x) {
  crs <- sf::st_crs(x)
  if (is.na(crs)) NULL else crs
}

sf_crs_from_wk <- function(x) {
  sf::st_crs(wk_crs(x))
}

#' @export
as_xy.sfc <- function(x, ...) {
  if (length(x) == 0) {
    xy(crs = wk_crs_from_sf(x))
  } else if (inherits(x, "sfc_POINT")) {
    coords <- sf::st_coordinates(x)
    dims <- colnames(coords)
    dimnames(coords) <- NULL

    if (identical(dims, c("X", "Y"))) {
      new_wk_xy(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE]
        ),
        crs = wk_crs_from_sf(x)
      )
    } else if (identical(dims, c("X", "Y", "Z"))) {
      new_wk_xyz(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          z = coords[, 3, drop = TRUE]
        ),
        crs = wk_crs_from_sf(x)
      )
    } else if (identical(dims, c("X", "Y", "M"))) {
      new_wk_xym(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          m = coords[, 3, drop = TRUE]
        ),
        crs = wk_crs_from_sf(x)
      )
    } else if (identical(dims, c("X", "Y", "Z", "M"))) {
      new_wk_xyzm(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          z = coords[, 3, drop = TRUE],
          m = coords[, 4, drop = TRUE]
        ),
        crs = wk_crs_from_sf(x)
      )
    } else {
      stop("Unknown dimensions.", call. = FALSE) # nocov
    }
  } else {
    NextMethod()
  }
}

#' @export
as_rct.bbox <- function(x, ...) {
  x_bare <- unclass(x)
  new_wk_rct(as.list(x_bare[c("xmin", "ymin", "xmax", "ymax")]), crs = wk_crs_from_sf(x))
}

#' @export
as_wkb.sf <- function(x, ...) {
  as_wkb(sf::st_geometry(x), ...)
}

#' @export
as_wkt.sf <- function(x, ...) {
  as_wkt(sf::st_geometry(x), ...)
}

#' @export
as_xy.sf <- function(x, ..., dims = NULL) {
  as_xy(sf::st_geometry(x), ..., dims = dims)
}

# dynamically exported
st_as_sfc.wk_wkb <- function(x, ...) {
  sf::st_set_crs(sf::st_as_sfc(structure(x, class = "WKB"), EWKB = TRUE), sf_crs_from_wk(x))
}

st_as_sf.wk_wkb <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_wkb(x, ...))
    )
  )
}

st_as_sfc.wk_wkt <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x), ...)
}

st_as_sf.wk_wkt <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_wkt(x, ...))
    )
  )
}

st_as_sfc.wk_xy <- function(x, ...) {
  if (all(!is.na(x))) {
    st_as_sf.wk_xy(x, ...)$geometry
  } else {
    sf::st_as_sfc(as_wkb(x), ...)
  }
}

st_as_sf.wk_xy <- function(x, ...) {
  if (all(!is.na(x))) {
    sf::st_as_sf(as.data.frame(x), coords = xy_dims(x), crs = sf_crs_from_wk(x))
  } else {
    sf::st_as_sf(
      new_data_frame(
        list(geometry = sf::st_as_sfc(as_wkb(x), ...))
      )
    )
  }
}

st_as_sfc.wk_rct <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x, ...))
}

st_as_sfc.wk_crc <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x, ...))
}

st_as_sf.wk_rct <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_rct(x, ...))
    )
  )
}

st_as_sf.wk_crc <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_crc(x, ...))
    )
  )
}
