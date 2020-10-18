
#' @export
as_wkb.sfc <- function(x, ..., include_srid = FALSE) {
  as_wkb(sf::st_as_binary(x, ..., precision = 0, EWKB = include_srid))
}

#' @export
as_wkt.sfc <- function(x, ..., include_srid = FALSE) {
  result <- as_wkt(as_wkb(x, ..., include_srid = include_srid), include_srid = include_srid)
  is_empty_point <- sf::st_is_empty(x) & (sf::st_geometry_type(x) == "POINT")
  result[is_empty_point] <- "POINT EMPTY"
  result
}

#' @export
as_wksxp.sfc <- function(x, ..., include_srid = FALSE) {
  wksxp <- as_wksxp(
    as_wkb(
      sf::st_as_binary(x, ..., precision = 0, EWKB = include_srid)
    )
  )

  is_empty_point <- sf::st_is_empty(x) & (sf::st_geometry_type(x) == "POINT")
  wksxp[is_empty_point] <- "POINT EMPTY"
  wksxp
}

#' @export
as_xy.sfc <- function(x, ...) {
  if (length(x) == 0) {
    xy()
  } else if (inherits(x, "sfc_POINT")) {
    coords <- sf::st_coordinates(x)
    dims <- colnames(coords)
    dimnames(coords) <- NULL

    if (identical(dims, c("X", "Y"))) {
      new_wk_xy(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE]
        )
      )
    } else if (identical(dims, c("X", "Y", "Z"))) {
      new_wk_xyz(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          z = coords[, 3, drop = TRUE]
        )
      )
    } else if (identical(dims, c("X", "Y", "M"))) {
      new_wk_xym(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          m = coords[, 3, drop = TRUE]
        )
      )
    } else if (identical(dims, c("X", "Y", "Z", "M"))) {
      new_wk_xyzm(
        list(
          x = coords[, 1, drop = TRUE],
          y = coords[, 2, drop = TRUE],
          z = coords[, 3, drop = TRUE],
          m = coords[, 4, drop = TRUE]
        )
      )
    } else {
      stop("Unknown dimensions.", call. = FALSE) # nocov
    }
  } else {
    stop(sprintf("Can't create xy() from sfc with class %s", class(x)[1]), call. = FALSE)
  }
}

#' @export
as_rct.bbox <- function(x, ...) {
  x <- unclass(x)
  new_wk_rct(as.list(x[c("xmin", "ymin", "xmax", "ymax")]))
}

#' @export
as_wkb.sf <- function(x, ..., include_srid = FALSE) {
  as_wkb(sf::st_geometry(x), ..., include_srid = include_srid)
}

#' @export
as_wkt.sf <- function(x, ..., include_srid = FALSE) {
  as_wkt(sf::st_geometry(x), ..., include_srid = include_srid)
}

#' @export
as_wksxp.sf <- function(x, ..., dims = NULL) {
  as_wksxp(sf::st_geometry(x), ..., dims = dims)
}

#' @export
as_xy.sf <- function(x, ..., dims = NULL) {
  as_xy(sf::st_geometry(x), ..., dims = dims)
}

# dynamically exported
st_as_sfc.wk_wkb <- function(x, ...) {
  sf::st_as_sfc(structure(x, class = "WKB"), EWKB = TRUE)
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

st_as_sfc.wk_wksxp <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x), ...)
}

st_as_sf.wk_wksxp <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_wksxp(x, ...))
    )
  )
}

st_as_sfc.wk_xy <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x), ...)
}

st_as_sf.wk_xy <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_xy(x, ...))
    )
  )
}

st_as_sfc.wk_rct <- function(x, ...) {
  sf::st_as_sfc(as_wkb(x), ...)
}

st_as_sf.wk_rct <- function(x, ...) {
  sf::st_as_sf(
    new_data_frame(
      list(geometry = st_as_sfc.wk_rct(x, ...))
    )
  )
}

