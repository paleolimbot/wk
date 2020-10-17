
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

vec_restore.wk_wkb <- function(x, ...) {
  new_wk_wkb(x)
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
  x
}

#' @method vec_cast.wk_wkb wk_wkt
#' @export
vec_cast.wk_wkb.wk_wkt <- function(x, to, ...) {
  as_wkb(x)
}

#' @method vec_cast.wk_wkb wk_wksxp
#' @export
vec_cast.wk_wkb.wk_wksxp <- function(x, to, ...) {
  as_wkb(x)
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
  new_wk_wkb()
}

#' @method vec_ptype2.wk_wkb wk_wkt
#' @export
vec_ptype2.wk_wkb.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt()
}

#' @method vec_ptype2.wk_wkb wk_wksxp
#' @export
vec_ptype2.wk_wkb.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp()
}

# wkt() --------

vec_proxy.wk_wkt <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkt <- function(x, ...) {
  new_wk_wkt(x)
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
  as_wkt(x)
}

#' @method vec_cast.wk_wkt wk_wksxp
#' @export
vec_cast.wk_wkt.wk_wksxp <- function(x, to, ...) {
  as_wkt(x)
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
  new_wk_wkt()
}

#' @method vec_ptype2.wk_wkt wk_wkb
#' @export
vec_ptype2.wk_wkt.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wkt()
}

#' @method vec_ptype2.wk_wkt wk_wksxp
#' @export
vec_ptype2.wk_wkt.wk_wksxp <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp()
}

# wksxp() --------

vec_proxy.wk_wksxp <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wksxp <- function(x, ...) {
  new_wk_wksxp(x)
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
  as_wksxp(x)
}

#' @method vec_cast.wk_wksxp wk_wkt
#' @export
vec_cast.wk_wksxp.wk_wkt <- function(x, to, ...) {
  as_wksxp(x)
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
  new_wk_wksxp()
}

#' @method vec_ptype2.wk_wksxp wk_wkb
#' @export
vec_ptype2.wk_wksxp.wk_wkb <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp()
}

#' @method vec_ptype2.wk_wksxp wk_wkt
#' @export
vec_ptype2.wk_wksxp.wk_wkt <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  new_wk_wksxp()
}

# xy() --------

vec_proxy.wk_xy <- function(x, ...) {
  as.data.frame(x)
}

vec_restore.wk_xy <- function(x, ...) {
  attr(x, "row.names") <- NULL
  new_wk_xy(x)
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

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xy
vec_ptype2.wk_xy <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xy", y)
}

# xyz() --------

vec_proxy.wk_xyz <- function(x, ...) {
  as.data.frame(x)
}

vec_restore.wk_xyz <- function(x, ...) {
  attr(x, "row.names") <- NULL
  new_wk_xyz(x)
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

#' @rdname vctrs-methods
#' @export vec_ptype2.wk_xyz
vec_ptype2.wk_xyz <- function(x, y, ...) {
  UseMethod("vec_ptype2.wk_xyz", y)
}

# xym() --------

vec_proxy.wk_xym <- function(x, ...) {
  as.data.frame(x)
}

vec_restore.wk_xym <- function(x, ...) {
  attr(x, "row.names") <- NULL
  new_wk_xym(x)
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

# xyzm() --------

vec_proxy.wk_xyzm <- function(x, ...) {
  as.data.frame(x)
}

vec_restore.wk_xyzm <- function(x, ...) {
  attr(x, "row.names") <- NULL
  new_wk_xyzm(x)
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

# rct() --------

vec_proxy.wk_rct <- function(x, ...) {
  as.data.frame(x)
}

vec_restore.wk_rct <- function(x, ...) {
  attr(x, "row.names") <- NULL
  new_wk_rct(x)
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
