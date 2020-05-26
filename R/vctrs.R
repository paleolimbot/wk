
vec_proxy.wk_wkb <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkb <- function(x, ...) {
  new_wk_wkb(x)
}

vec_cast.wk_wkb <- function(x, to, ...) {
  UseMethod("vec_cast.wk_wkb")
}

vec_cast.wk_wkb.default <- function(x, to, ...) {
  vctrs::vec_default_cast(x, to)
}

vec_cast.wk_wkb.wk_wkb <- function(x, to, ...) {
  x
}

# nocov start
.onLoad <- function(...) {
  # register vctrs dependencies
  register_s3_method("vctrs", "vec_proxy", "wk_wkb")
  register_s3_method("vctrs", "vec_restore", "wk_wkb")
  register_s3_method("vctrs", "vec_cast", "wk_wkb")
  register_s3_method("wk", "vec_cast.wk_wkb", "default")
  register_s3_method("wk", "vec_cast.wk_wkb", "wk_wkb")
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
