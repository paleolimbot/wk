
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

#' @method vec_cast.wk_wkt default
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

# .onLoad() -----------------

# nocov start
.onLoad <- function(...) {
  # register vctrs dependencies
  for (cls in c("wk_wkb", "wk_wkt", "wk_wksxp")) {
    register_s3_method("vctrs", "vec_proxy", cls)
    register_s3_method("vctrs", "vec_restore", cls)
    register_s3_method("vctrs", "vec_cast", cls)
    register_s3_method("vctrs", "vec_ptype2", cls)
  }
}

register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(is.character(pkg), length(pkg) == 1)
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
# nocov end
