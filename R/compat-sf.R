
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
as_wkb.sf <- function(x, ..., include_srid = FALSE) {
  as_wkb(sf::st_geometry(x), ..., include_srid = include_srid)
}

#' @export
as_wkt.sf <- function(x, ..., include_srid = FALSE) {
  as_wkt(sf::st_geometry(x), ..., include_srid = include_srid)
}

#' @export
as_wksxp.sf <- function(x, ..., include_srid = FALSE) {
  as_wksxp(sf::st_geometry(x), ..., include_srid = include_srid)
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
