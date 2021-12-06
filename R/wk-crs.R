
#' Set and get vector CRS
#'
#' The wk package doesn't operate on CRS objects, but does propagate them
#' through subsetting and concatenation. A CRS object can be any R object,
#' and x can be any object whose 'crs' attribute carries a CRS. These functions
#' are S3 generics to keep them from being used
#' on objects that do not use this system of CRS propagation.
#'
#' @param x,... Objects whose "crs" attribute is used to carry a CRS.
#' @param crs,value An object that can be interpreted as a CRS
#'
#' @export
#'
wk_crs <- function(x) {
  UseMethod("wk_crs")
}

#' @rdname wk_crs
#' @export
wk_crs.wk_vctr <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @rdname wk_crs
#' @export
wk_crs.wk_rcrd <- function(x) {
  attr(x, "crs", exact = TRUE)
}

#' @rdname wk_crs
#' @export
`wk_crs<-` <- function(x, value) {
  wk_set_crs(x, value)
}

#' @rdname wk_crs
#' @export
wk_set_crs <- function(x, crs) {
  UseMethod("wk_set_crs")
}

#' @export
wk_set_crs.wk_vctr <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @export
wk_set_crs.wk_rcrd <- function(x, crs) {
  attr(x, "crs") <- crs
  x
}

#' @rdname wk_crs
#' @export
wk_crs_output <- function(...) {
  dots <- list(...)
  crs <- lapply(dots, wk_crs)
  Reduce(wk_crs2, crs)
}

#' @rdname wk_crs
#' @export
wk_is_geodesic_output <- function(...) {
  dots <- list(...)
  geodesic <- lapply(dots, wk_is_geodesic)
  Reduce(wk_is_geodesic2, geodesic)
}

wk_crs2 <- function(x, y) {
  if (inherits(y, "wk_crs_inherit")) {
    x
  } else if (inherits(x, "wk_crs_inherit")) {
    y
  } else if (wk_crs_equal(x, y)) {
    x
  } else {
    stop(sprintf("CRS objects '%s' and '%s' are not equal.", format(x), format(y)), call. = FALSE)
  }
}

wk_is_geodesic2 <- function(x, y) {
  if (identical(x, y)) {
    x
  } else {
    stop("objects have differing values for geodesic", call. = FALSE)
  }
}

#' Compare CRS objects
#'
#' The [wk_crs_equal()] function uses special S3 dispatch on [wk_crs_equal_generic()]
#' to evaluate whether or not two CRS values can be considered equal. When implementing
#' [wk_crs_equal_generic()], every attempt should be made to make `wk_crs_equal(x, y)`
#' and `wk_crs_equal(y, x)` return identically.
#'
#' @param x,y Objects stored in the `crs` attribute of a vector.
#' @param ... Unused
#'
#' @return `TRUE` if `x` and `y` can be considered equal, `FALSE` otherwise.
#' @export
#'
wk_crs_equal <- function(x, y) {
  if (is.object(y)) {
    wk_crs_equal_generic(y, x)
  } else {
    wk_crs_equal_generic(x, y)
  }
}

#' @rdname wk_crs_equal
#' @export
wk_crs_equal_generic <- function(x, y, ...) {
  UseMethod("wk_crs_equal_generic")
}

#' @export
wk_crs_equal_generic.default <- function(x, y, ...) {
  identical(x, y)
}

#' @export
wk_crs_equal_generic.integer <- function(x, y, ...) {
  isTRUE(x == y)
}

#' @export
wk_crs_equal_generic.double <- function(x, y, ...) {
  isTRUE(x == y)
}


#' Set and get vector geodesic edge interpolation
#'
#' @param x An R object that contains edges
#' @param geodesic,value `TRUE` if edges must be interpolated as geodesics when
#'   coordinates are spherical, `FALSE` otherwise.
#'
#' @return `TRUE` if edges must be interpolated as geodesics when
#'   coordinates are spherical, `FALSE` otherwise.
#' @export
#'
wk_is_geodesic <- function(x) {
  UseMethod("wk_is_geodesic")
}

#' @rdname wk_is_geodesic
#' @export
wk_set_geodesic <- function(x, geodesic) {
  UseMethod("wk_set_geodesic")
}

#' @rdname wk_is_geodesic
#' @export
`wk_is_geodesic<-` <- function(x, value) {
 wk_set_geodesic(x, value)
}

#' @export
wk_is_geodesic.default <- function(x) {
  FALSE
}

#' @export
wk_is_geodesic.wk_wkb <- function(x) {
  attr(x, "geodesic", exact = TRUE) %||% FALSE
}

#' @export
wk_is_geodesic.wk_wkt <- function(x) {
  attr(x, "geodesic", exact = TRUE) %||% FALSE
}

#' @export
wk_is_geodesic.wk_rct <- function(x) {
  attr(x, "geodesic", exact = TRUE) %||% FALSE
}

#' @export
wk_set_geodesic.wk_wkb <- function(x, geodesic) {
  attr(x, "geodesic") <- if (isTRUE(geodesic)) TRUE else NULL
  x
}

#' @export
wk_set_geodesic.wk_wkt <- function(x, geodesic) {
  attr(x, "geodesic") <- if (isTRUE(geodesic)) TRUE else NULL
  x
}

#' @export
wk_set_geodesic.wk_rct <- function(x, geodesic) {
  attr(x, "geodesic") <- if (isTRUE(geodesic)) TRUE else NULL
  x
}

#' CRS object generic methods
#'
#' @param crs An arbitrary R object
#' @param verbose Use `TRUE` to request a more verbose version of the
#'   PROJ definition (e.g., WKT2). The default of `FALSE` should return
#'   the most compact version that completely describes the CRS. An
#'   authority:code string (e.g., "OGC:CRS84") is the recommended way
#'   to represent a CRS when `verbose` is `FALSE`, if possible, falling
#'   back to the most recent version of WKT2.
#' @param proj_version A [package_version()] of the PROJ version, or
#'   `NULL` if the PROJ version is unknown.
#'
#' @return
#'   - `wk_crs_proj_definition()` Returns a string used to represent the
#'     CRS in PROJ. For recent PROJ version you'll want to return WKT2; however
#'     you should check `proj_version` if you want this to work with older
#'     versions of PROJ.
#' @export
#'
#' @examples
#' wk_crs_proj_definition("EPSG:4326")
#'
wk_crs_proj_definition <- function(crs, proj_version = NULL, verbose = FALSE) {
  UseMethod("wk_crs_proj_definition")
}

#' @rdname wk_crs_proj_definition
#' @export
wk_crs_proj_definition.NULL <- function(crs, proj_version = NULL, verbose = FALSE) {
  NA_character_
}

#' @rdname wk_crs_proj_definition
#' @export
wk_crs_proj_definition.character <- function(crs, proj_version = NULL, verbose = FALSE) {
  stopifnot(length(crs) == 1)
  crs
}

#' @rdname wk_crs_proj_definition
#' @export
wk_crs_proj_definition.double <- function(crs, proj_version = NULL, verbose = FALSE) {
  stopifnot(length(crs) == 1)
  if (is.na(crs)) wk_crs_proj_definition(NULL) else paste0("EPSG:", crs)
}

#' @rdname wk_crs_proj_definition
#' @export
wk_crs_proj_definition.integer <- function(crs, proj_version = NULL, verbose = FALSE) {
  stopifnot(length(crs) == 1)
  if (is.na(crs)) wk_crs_proj_definition(NULL) else paste0("EPSG:", crs)
}

#' Special CRS values
#'
#' The CRS handling in the wk package requires two sentinel CRS values.
#' The first, [wk_crs_inherit()], signals that the vector should inherit
#' a CRS of another vector if combined. This is useful for empty, `NULL`,
#' and/or zero-length geometries. The second, [wk_crs_auto()], is used
#' as the default argument of `crs` for constructors so that zero-length
#' geometries are assigned a CRS of `wk_crs_inherit()` by default.
#'
#' @param x A raw input to a construuctor whose length and crs attributte
#'   is used to determine the default CRS returned by [wk_crs_auto()].
#' @param crs A value for the coordinate reference system supplied by
#'   the user.
#'
#' @export
#'
#' @examples
#' wk_crs_auto_value(list(), wk_crs_auto())
#' wk_crs_auto_value(list(), 1234)
#' wk_crs_auto_value(list(NULL), wk_crs_auto())
#'
wk_crs_inherit <- function() {
  structure(list(), class = "wk_crs_inherit")
}

#' @rdname wk_crs_inherit
#' @export
wk_crs_longlat <- function(crs = NULL) {
  if (inherits(crs, "wk_crs_inherit") || is.null(crs) || identical(crs, "WGS84")) {
    return("OGC:CRS84")
  }

  crs_proj <- wk_crs_proj_definition(crs)
  switch(
    crs_proj,
    "OGC:CRS84" = ,
    "EPSG:4326" = ,
    "WGS84" = "OGC:CRS84",
    "OGC:CRS27" = ,
    "EPSG:4267" = ,
    "NAD27" = "OGC:CRS27",
    "OGC:CRS83" = ,
    "EPSG:4269" = ,
    "NAD83" = "OGC:CRS83",
    stop(
      sprintf(
        "Can't guess authority-compliant long/lat definition from CRS '%s'",
        format(crs_proj)
      )
    )
  )
}

#' @rdname wk_crs_inherit
#' @export
wk_crs_auto <- function() {
  structure(list(), class = "wk_crs_auto")
}

#' @rdname wk_crs_inherit
#' @export
wk_crs_auto_value <- function(x, crs) {
  if (inherits(crs, "wk_crs_auto")) {
    if (length(x) == 0) wk_crs_inherit() else attr(x, "crs", exact = TRUE)
  } else {
    crs
  }
}

#' @export
format.wk_crs_inherit <- function(x, ...) {
  format("wk_crs_inherit()", ...)
}

#' @export
print.wk_crs_inherit <- function(x, ...) {
  cat("<wk_crs_inherit>\n")
}

wk_crs_format <- function(x, ...) {
  tryCatch(
    wk_crs_proj_definition(x, verbose = FALSE),
    error = function(e) format(x, ...)
  )
}

