
#' Vctrs methods
#'
#' @param x,y,to,... See [vctrs::vec_cast()] and [vctrs::vec_ptype2()].
#' @rdname vctrs-methods
#' @name vctrs-methods
#'
NULL

# wkb() --------

vec_proxy.wk_wkb <- function(x, ...) {
  unclass(x)
}

vec_proxy_equal.wk_wkb <- function(x, ...) {
  wkb_to_hex(x)
}

vec_restore.wk_wkb <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  geodesic_out <- attr(to, "geodesic", exact = TRUE) %||% attr(x, "geodesic", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "geodesic") <- NULL
  new_wk_wkb(x, crs = crs_out, geodesic = geodesic_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_wkb
vec_cast.wk_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkb") # nocov
}

#' @method vec_cast.wk_wkb default
#' @export
vec_cast.wk_wkb.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_wkb wk_wkb
#' @export
vec_cast.wk_wkb.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  x
}

#' @method vec_cast.wk_wkb wk_wkt
#' @export
vec_cast.wk_wkb.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  as_wkb(x)
}

#' @method vec_cast.wk_wkb wk_xy
#' @export
vec_cast.wk_wkb.wk_xy <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkb wk_xyz
#' @export
vec_cast.wk_wkb.wk_xyz <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkb wk_xym
#' @export
vec_cast.wk_wkb.wk_xym <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkb wk_xyzm
#' @export
vec_cast.wk_wkb.wk_xyzm <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkb wk_rct
#' @export
vec_cast.wk_wkb.wk_rct <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkb wk_crc
#' @export
vec_cast.wk_wkb.wk_crc <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_wkb(x)
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_wkb
vec_ptype2.wk_wkb <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkb", y) # nocov
}

#' @method vec_ptype2.wk_wkb default
#' @export
vec_ptype2.wk_wkb.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkb wk_wkb
#' @export
vec_ptype2.wk_wkb.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = geodesic_attr(wk_is_geodesic_output(x, y)))
}

#' @method vec_ptype2.wk_wkb wk_wkt
#' @export
vec_ptype2.wk_wkb.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = geodesic_attr(wk_is_geodesic_output(x, y)))
}

#' @method vec_ptype2.wk_wkb wk_xy
#' @export
vec_ptype2.wk_wkb.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkb wk_xyz
#' @export
vec_ptype2.wk_wkb.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkb wk_xym
#' @export
vec_ptype2.wk_wkb.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkb wk_xyzm
#' @export
vec_ptype2.wk_wkb.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkb wk_rct
#' @export
vec_ptype2.wk_wkb.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = geodesic_attr(wk_is_geodesic_output(x, y)))
}

#' @method vec_ptype2.wk_wkb wk_crc
#' @export
vec_ptype2.wk_wkb.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

# wkt() --------

vec_proxy.wk_wkt <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkt <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  geodesic_out <- attr(to, "geodesic", exact = TRUE) %||% attr(x, "geodesic", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "geodesic") <- NULL
  new_wk_wkt(x, crs = crs_out, geodesic = geodesic_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_wkt
vec_cast.wk_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkt") # nocov
}

#' @method vec_cast.wk_wkt default
#' @export
vec_cast.wk_wkt.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_wkt wk_wkt
#' @export
vec_cast.wk_wkt.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  x
}

#' @method vec_cast.wk_wkt wk_wkb
#' @export
vec_cast.wk_wkt.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  as_wkt(x)
}

#' @method vec_cast.wk_wkt wk_xy
#' @export
vec_cast.wk_wkt.wk_xy <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkt wk_xyz
#' @export
vec_cast.wk_wkt.wk_xyz <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkt wk_xym
#' @export
vec_cast.wk_wkt.wk_xym <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkt wk_xyzm
#' @export
vec_cast.wk_wkt.wk_xyzm <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkt wk_rct
#' @export
vec_cast.wk_wkt.wk_rct <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @method vec_cast.wk_wkt wk_crc
#' @export
vec_cast.wk_wkt.wk_crc <- function(x, to, ...) {
  wk_translate(x, to)
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_wkt
vec_ptype2.wk_wkt <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkt", y) # nocov
}

#' @method vec_ptype2.wk_wkt default
#' @export
vec_ptype2.wk_wkt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkt wk_wkt
#' @export
vec_ptype2.wk_wkt.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = geodesic_attr(wk_is_geodesic_output(x, y)))
}

#' @method vec_ptype2.wk_wkt wk_wkb
#' @export
vec_ptype2.wk_wkt.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = geodesic_attr(wk_is_geodesic_output(x, y)))
}

#' @method vec_ptype2.wk_wkt wk_xy
#' @export
vec_ptype2.wk_wkt.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkt wk_xyz
#' @export
vec_ptype2.wk_wkt.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkt wk_xym
#' @export
vec_ptype2.wk_wkt.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkt wk_xyzm
#' @export
vec_ptype2.wk_wkt.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_wkt wk_rct
#' @export
vec_ptype2.wk_wkt.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  wk_is_geodesic_output(x, y)
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_crc
#' @export
vec_ptype2.wk_wkt.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(x, "geodesic", exact = TRUE))
}

# xy() --------

vec_proxy.wk_xy <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_xy <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_xy(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_xy
vec_cast.wk_xy <- function(x, to, ...) {
  UseMethod("vec_cast.wk_xy") # nocov
}

#' @method vec_cast.wk_xy default
#' @export
vec_cast.wk_xy.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_xy wk_xy
#' @export
vec_cast.wk_xy.wk_xy <- function(x, to, ...) {
  wk_crs_output(x, to)
  x
}

#' @method vec_cast.wk_xy wk_wkb
#' @export
vec_cast.wk_xy.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x)
}

#' @method vec_cast.wk_xy wk_wkt
#' @export
vec_cast.wk_xy.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x)
}

#' @method vec_cast.wk_xy wk_xyz
#' @export
vec_cast.wk_xy.wk_xyz <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y")),
    x, to,
    !is.na(unclass(x)$z)
  )
}

#' @method vec_cast.wk_xy wk_xym
#' @export
vec_cast.wk_xy.wk_xym <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y")),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @method vec_cast.wk_xy wk_xyzm
#' @export
vec_cast.wk_xy.wk_xyzm <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y")),
    x, to,
    !is.na(unclass(x)$z) & !is.na(unclass(x)$m)
  )
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xy
vec_ptype2.wk_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xy", y) # nocov
}

#' @method vec_ptype2.wk_xy wk_xy
#' @export
vec_ptype2.wk_xy.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xy(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_wkb
#' @export
vec_ptype2.wk_xy.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xy wk_wkt
#' @export
vec_ptype2.wk_xy.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xy wk_xyz
#' @export
vec_ptype2.wk_xy.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyz(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_xym
#' @export
vec_ptype2.wk_xy.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xym(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_xyzm
#' @export
vec_ptype2.wk_xy.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_rct
#' @export
vec_ptype2.wk_xy.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_crc
#' @export
vec_ptype2.wk_xy.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# xyz() --------

vec_proxy.wk_xyz <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_xyz <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_xyz(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_xyz
vec_cast.wk_xyz <- function(x, to, ...) {
  UseMethod("vec_cast.wk_xyz") # nocov
}

#' @method vec_cast.wk_xyz default
#' @export
vec_cast.wk_xyz.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_xyz wk_xyz
#' @export
vec_cast.wk_xyz.wk_xyz <- function(x, to, ...) {
  wk_crs_output(x, to)
  x
}

#' @method vec_cast.wk_xyz wk_wkb
#' @export
vec_cast.wk_xyz.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z"))
}

#' @method vec_cast.wk_xyz wk_wkt
#' @export
vec_cast.wk_xyz.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z"))
}

#' @method vec_cast.wk_xyz wk_xy
#' @export
vec_cast.wk_xyz.wk_xy <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z"))
}

#' @method vec_cast.wk_xyz wk_xym
#' @export
vec_cast.wk_xyz.wk_xym <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "z")),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @method vec_cast.wk_xyz wk_xyzm
#' @export
vec_cast.wk_xyz.wk_xyzm <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "z")),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xyz
vec_ptype2.wk_xyz <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xyz", y) # nocov
}

#' @method vec_ptype2.wk_xyz wk_xyz
#' @export
vec_ptype2.wk_xyz.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyz(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_wkb
#' @export
vec_ptype2.wk_xyz.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xyz wk_wkt
#' @export
vec_ptype2.wk_xyz.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xyz wk_xy
#' @export
vec_ptype2.wk_xyz.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyz(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_xym
#' @export
vec_ptype2.wk_xyz.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_xyzm
#' @export
vec_ptype2.wk_xyz.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_rct
#' @export
vec_ptype2.wk_xyz.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_crc
#' @export
vec_ptype2.wk_xyz.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# xym() --------

vec_proxy.wk_xym <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_xym <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_xym(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_xym
vec_cast.wk_xym <- function(x, to, ...) {
  UseMethod("vec_cast.wk_xym") # nocov
}

#' @method vec_cast.wk_xym default
#' @export
vec_cast.wk_xym.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_xym wk_xym
#' @export
vec_cast.wk_xym.wk_xym <- function(x, to, ...) {
  wk_crs_output(x, to)
  x
}

#' @method vec_cast.wk_xym wk_wkb
#' @export
vec_cast.wk_xym.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "m"))
}

#' @method vec_cast.wk_xym wk_wkt
#' @export
vec_cast.wk_xym.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "m"))
}

#' @method vec_cast.wk_xym wk_xy
#' @export
vec_cast.wk_xym.wk_xy <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "m"))
}

#' @method vec_cast.wk_xym wk_xyz
#' @export
vec_cast.wk_xym.wk_xyz <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "m")),
    x, to,
    !is.na(unclass(x)$z)
  )
}

#' @method vec_cast.wk_xym wk_xyzm
#' @export
vec_cast.wk_xym.wk_xyzm <- function(x, to, ...) {
  wk_crs_output(x, to)
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "m")),
    x, to,
    !is.na(unclass(x)$z)
  )
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xym
vec_ptype2.wk_xym <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xym", y) # nocov
}

#' @method vec_ptype2.wk_xym wk_xym
#' @export
vec_ptype2.wk_xym.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xym(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_wkb
#' @export
vec_ptype2.wk_xym.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xym wk_wkt
#' @export
vec_ptype2.wk_xym.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xym wk_xy
#' @export
vec_ptype2.wk_xym.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xym(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_xyz
#' @export
vec_ptype2.wk_xym.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_xyzm
#' @export
vec_ptype2.wk_xym.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_rct
#' @export
vec_ptype2.wk_xym.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_crc
#' @export
vec_ptype2.wk_xym.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# xyzm() --------

vec_proxy.wk_xyzm <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_xyzm <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_xyzm(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_xyzm
vec_cast.wk_xyzm <- function(x, to, ...) {
  UseMethod("vec_cast.wk_xyzm") # nocov
}

#' @method vec_cast.wk_xyzm default
#' @export
vec_cast.wk_xyzm.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_xyzm wk_xyzm
#' @export
vec_cast.wk_xyzm.wk_xyzm <- function(x, to, ...) {
  wk_crs_output(x, to)
  x
}

#' @method vec_cast.wk_xyzm wk_wkb
#' @export
vec_cast.wk_xyzm.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z", "m"))
}

#' @method vec_cast.wk_xyzm wk_wkt
#' @export
vec_cast.wk_xyzm.wk_wkt <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z", "m"))
}

#' @method vec_cast.wk_xyzm wk_xy
#' @export
vec_cast.wk_xyzm.wk_xy <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z", "m"))
}

#' @method vec_cast.wk_xyzm wk_xyz
#' @export
vec_cast.wk_xyzm.wk_xyz <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z", "m"))
}

#' @method vec_cast.wk_xyzm wk_xym
#' @export
vec_cast.wk_xyzm.wk_xym <- function(x, to, ...) {
  wk_crs_output(x, to)
  as_xy(x, dims = c("x", "y", "z", "m"))
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xyzm
vec_ptype2.wk_xyzm <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xyzm", y) # nocov
}

#' @method vec_ptype2.wk_xyzm wk_xyzm
#' @export
vec_ptype2.wk_xyzm.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_wkb
#' @export
vec_ptype2.wk_xyzm.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xyzm wk_wkt
#' @export
vec_ptype2.wk_xyzm.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_xyzm wk_xy
#' @export
vec_ptype2.wk_xyzm.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_xyz
#' @export
vec_ptype2.wk_xyzm.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_xym
#' @export
vec_ptype2.wk_xyzm.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_rct
#' @export
vec_ptype2.wk_xyzm.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_crc
#' @export
vec_ptype2.wk_xyzm.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# rct() --------

vec_proxy.wk_rct <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_rct <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_rct(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_rct
vec_cast.wk_rct <- function(x, to, ...) {
  UseMethod("vec_cast.wk_rct") # nocov
}

#' @method vec_cast.wk_rct default
#' @export
vec_cast.wk_rct.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_rct
vec_ptype2.wk_rct <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_rct", y) # nocov
}

#' @method vec_cast.wk_rct wk_rct
#' @export
vec_cast.wk_rct.wk_rct <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  x
}

#' @method vec_cast.wk_rct wk_wkb
#' @export
vec_cast.wk_rct.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  as_wkb(x, ...)
}

#' @method vec_ptype2.wk_rct wk_rct
#' @export
vec_ptype2.wk_rct.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_rct(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_wkb
#' @export
vec_ptype2.wk_rct.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_wkt
#' @export
vec_ptype2.wk_rct.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_xy
#' @export
vec_ptype2.wk_rct.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_xyz
#' @export
vec_ptype2.wk_rct.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_xym
#' @export
vec_ptype2.wk_rct.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_xyzm
#' @export
vec_ptype2.wk_rct.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_rct wk_crc
#' @export
vec_ptype2.wk_rct.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# crc() --------

vec_proxy.wk_crc <- function(x, ...) {
  new_data_frame(unclass(x))
}

vec_restore.wk_crc <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  attr(x, "row.names") <- NULL
  new_wk_crc(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_crc
vec_cast.wk_crc <- function(x, to, ...) {
  UseMethod("vec_cast.wk_crc") # nocov
}

#' @method vec_cast.wk_crc default
#' @export
vec_cast.wk_crc.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_crc wk_crc
#' @export
vec_cast.wk_crc.wk_crc <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  x
}

#' @method vec_cast.wk_crc wk_wkb
#' @export
vec_cast.wk_crc.wk_wkb <- function(x, to, ...) {
  wk_crs_output(x, to)
  wk_is_geodesic_output(x, to)
  as_wkb(x, ...)
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_crc
vec_ptype2.wk_crc <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_crc", y) # nocov
}

#' @method vec_ptype2.wk_crc wk_crc
#' @export
vec_ptype2.wk_crc.wk_crc <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_crc(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_crc wk_wkb
#' @export
vec_ptype2.wk_crc.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_crc wk_wkt
#' @export
vec_ptype2.wk_crc.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y), geodesic = attr(y, "geodesic", exact = TRUE))
}

#' @method vec_ptype2.wk_crc wk_xy
#' @export
vec_ptype2.wk_crc.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_crc wk_xyz
#' @export
vec_ptype2.wk_crc.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_crc wk_xym
#' @export
vec_ptype2.wk_crc.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_crc wk_xyzm
#' @export
vec_ptype2.wk_crc.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}
