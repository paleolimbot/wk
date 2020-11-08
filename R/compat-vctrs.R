
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

vec_restore.wk_wkb <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_wk_wkb(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_wkb
vec_cast.wk_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkb")
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
  x
}

#' @method vec_cast.wk_wkb wk_wkt
#' @export
vec_cast.wk_wkb.wk_wkt <- function(x, to, ...) {
  as_wkb(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_wkb wk_wksxp
#' @export
vec_cast.wk_wkb.wk_wksxp <- function(x, to, ...) {
  as_wkb(x, crs = wk_crs_output(x, to))
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_wkb
vec_ptype2.wk_wkb <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkb", y)
}

#' @method vec_ptype2.wk_wkb default
#' @export
vec_ptype2.wk_wkb.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkb wk_wkb
#' @export
vec_ptype2.wk_wkb.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_wkt
#' @export
vec_ptype2.wk_wkb.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_wksxp
#' @export
vec_ptype2.wk_wkb.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_xy
#' @export
vec_ptype2.wk_wkb.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_xyz
#' @export
vec_ptype2.wk_wkb.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_xym
#' @export
vec_ptype2.wk_wkb.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_xyzm
#' @export
vec_ptype2.wk_wkb.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkb wk_rct
#' @export
vec_ptype2.wk_wkb.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

# wkt() --------

vec_proxy.wk_wkt <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkt <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_wk_wkt(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_wkt
vec_cast.wk_wkt <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkt")
}

#' @method vec_cast.wk_wkt default
#' @export
vec_cast.wk_wkt.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_wkt wk_wkt
#' @export
vec_cast.wk_wkt.wk_wkt <- function(x, to, ...) {
  x
}

#' @method vec_cast.wk_wkt wk_wkb
#' @export
vec_cast.wk_wkt.wk_wkb <- function(x, to, ...) {
  as_wkt(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_wkt wk_wksxp
#' @export
vec_cast.wk_wkt.wk_wksxp <- function(x, to, ...) {
  as_wkt(x, crs = wk_crs_output(x, to))
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_wkt
vec_ptype2.wk_wkt <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wkt", y)
}

#' @method vec_ptype2.wk_wkt default
#' @export
vec_ptype2.wk_wkt.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wkt wk_wkt
#' @export
vec_ptype2.wk_wkt.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_wkb
#' @export
vec_ptype2.wk_wkt.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_wksxp
#' @export
vec_ptype2.wk_wkt.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_xy
#' @export
vec_ptype2.wk_wkt.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_xyz
#' @export
vec_ptype2.wk_wkt.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_xym
#' @export
vec_ptype2.wk_wkt.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_xyzm
#' @export
vec_ptype2.wk_wkt.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wkt wk_rct
#' @export
vec_ptype2.wk_wkt.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

# wksxp() --------

vec_proxy.wk_wksxp <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wksxp <- function(x, to, ...) {
  crs_out <- attr(to, "crs", exact = TRUE) %||% attr(x, "crs", exact = TRUE)
  attr(x, "crs") <- NULL
  new_wk_wksxp(x, crs = crs_out)
}

#' @rdname vctrs-methods
#' @export vec_cast.wk_wksxp
vec_cast.wk_wksxp <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wksxp")
}

#' @method vec_cast.wk_wksxp default
#' @export
vec_cast.wk_wksxp.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_wksxp wk_wksxp
#' @export
vec_cast.wk_wksxp.wk_wksxp <- function(x, to, ...) {
  x
}

#' @method vec_cast.wk_wksxp wk_wkb
#' @export
vec_cast.wk_wksxp.wk_wkb <- function(x, to, ...) {
  as_wksxp(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_wksxp wk_wkt
#' @export
vec_cast.wk_wksxp.wk_wkt <- function(x, to, ...) {
  as_wksxp(x, crs = wk_crs_output(x, to))
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_wksxp
vec_ptype2.wk_wksxp <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_wksxp", y)
}

#' @method vec_ptype2.wk_wksxp default
#' @export
vec_ptype2.wk_wksxp.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg) # nocov
}

#' @method vec_ptype2.wk_wksxp wk_wksxp
#' @export
vec_ptype2.wk_wksxp.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_wkb
#' @export
vec_ptype2.wk_wksxp.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_wkt
#' @export
vec_ptype2.wk_wksxp.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_xy
#' @export
vec_ptype2.wk_wksxp.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_xyz
#' @export
vec_ptype2.wk_wksxp.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_xym
#' @export
vec_ptype2.wk_wksxp.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_xyzm
#' @export
vec_ptype2.wk_wksxp.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_wksxp wk_rct
#' @export
vec_ptype2.wk_wksxp.wk_rct <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
}

# xy() --------

vec_proxy.wk_xy <- function(x, ...) {
  as.data.frame(x)
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
  UseMethod("vec_cast.wk_xy")
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
  as_xy(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xy wk_wkt
#' @export
vec_cast.wk_xy.wk_wkt <- function(x, to, ...) {
  as_xy(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xy wk_wksxp
#' @export
vec_cast.wk_xy.wk_wksxp <- function(x, to, ...) {
  as_xy(x, crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xy wk_xyz
#' @export
vec_cast.wk_xy.wk_xyz <- function(x, to, ...) {
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y"), crs = wk_crs_output(x, to)),
    x, to,
    !is.na(unclass(x)$z)
  )
}

#' @method vec_cast.wk_xy wk_xym
#' @export
vec_cast.wk_xy.wk_xym <- function(x, to, ...) {
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y"), crs = wk_crs_output(x, to)),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @method vec_cast.wk_xy wk_xyzm
#' @export
vec_cast.wk_xy.wk_xyzm <- function(x, to, ...) {
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y"), crs = wk_crs_output(x, to)),
    x, to,
    !is.na(unclass(x)$z) & !is.na(unclass(x)$m)
  )
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xy
vec_ptype2.wk_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xy", y)
}

#' @method vec_ptype2.wk_xy wk_xy
#' @export
vec_ptype2.wk_xy.wk_xy <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xy(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_wkb
#' @export
vec_ptype2.wk_xy.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_wkt
#' @export
vec_ptype2.wk_xy.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xy wk_wksxp
#' @export
vec_ptype2.wk_xy.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
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

# xyz() --------

vec_proxy.wk_xyz <- function(x, ...) {
  as.data.frame(x)
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
  UseMethod("vec_cast.wk_xyz")
}

#' @method vec_cast.wk_xyz default
#' @export
vec_cast.wk_xyz.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @method vec_cast.wk_xyz wk_wkb
#' @export
vec_cast.wk_xyz.wk_wkb <- function(x, to, ...) {
  as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xyz wk_wkt
#' @export
vec_cast.wk_xyz.wk_wkt <- function(x, to, ...) {
  as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xyz wk_wksxp
#' @export
vec_cast.wk_xyz.wk_wksxp <- function(x, to, ...) {
  as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xyz wk_xy
#' @export
vec_cast.wk_xyz.wk_xy <- function(x, to, ...) {
  as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to))
}

#' @method vec_cast.wk_xyz wk_xym
#' @export
vec_cast.wk_xyz.wk_xym <- function(x, to, ...) {
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to)),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @method vec_cast.wk_xyz wk_xyzm
#' @export
vec_cast.wk_xyz.wk_xyzm <- function(x, to, ...) {
  vctrs::maybe_lossy_cast(
    as_xy(x, dims = c("x", "y", "z"), crs = wk_crs_output(x, to)),
    x, to,
    !is.na(unclass(x)$m)
  )
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xyz
vec_ptype2.wk_xyz <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xyz", y)
}

#' @method vec_ptype2.wk_xyz wk_xyz
#' @export
vec_ptype2.wk_xyz.wk_xyz <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyz(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_wkb
#' @export
vec_ptype2.wk_xyz.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_wkt
#' @export
vec_ptype2.wk_xyz.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyz wk_wksxp
#' @export
vec_ptype2.wk_xyz.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
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

# xym() --------

vec_proxy.wk_xym <- function(x, ...) {
  as.data.frame(x)
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
  UseMethod("vec_cast.wk_xym")
}

#' @method vec_cast.wk_xym default
#' @export
vec_cast.wk_xym.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xym
vec_ptype2.wk_xym <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xym", y)
}

#' @method vec_ptype2.wk_xym wk_xym
#' @export
vec_ptype2.wk_xym.wk_xym <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xym(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_wkb
#' @export
vec_ptype2.wk_xym.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_wkt
#' @export
vec_ptype2.wk_xym.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xym wk_wksxp
#' @export
vec_ptype2.wk_xym.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
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

# xyzm() --------

vec_proxy.wk_xyzm <- function(x, ...) {
  as.data.frame(x)
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
  UseMethod("vec_cast.wk_xyzm")
}

#' @method vec_cast.wk_xyzm default
#' @export
vec_cast.wk_xyzm.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xyzm
vec_ptype2.wk_xyzm <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xyzm", y)
}

#' @method vec_ptype2.wk_xyzm wk_xyzm
#' @export
vec_ptype2.wk_xyzm.wk_xyzm <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_xyzm(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_wkb
#' @export
vec_ptype2.wk_xyzm.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkb(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_wkt
#' @export
vec_ptype2.wk_xyzm.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt(crs = wk_crs_output(x, y))
}

#' @method vec_ptype2.wk_xyzm wk_wksxp
#' @export
vec_ptype2.wk_xyzm.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
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

# rct() --------

vec_proxy.wk_rct <- function(x, ...) {
  as.data.frame(x)
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
  UseMethod("vec_cast.wk_rct")
}

#' @method vec_cast.wk_rct default
#' @export
vec_cast.wk_rct.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to) # nocov
}

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_rct
vec_ptype2.wk_rct <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_rct", y)
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

#' @method vec_ptype2.wk_rct wk_wksxp
#' @export
vec_ptype2.wk_rct.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp(crs = wk_crs_output(x, y))
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
