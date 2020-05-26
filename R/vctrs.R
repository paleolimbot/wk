
vec_proxy.wk_wkb <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkb <- function(x, ...) {
  new_wk_wkb(x)
}

vec_cast.wk_wkb <- function(x, to, ...) {
  as_wkb(x)
}

vec_proxy.wk_wkt <- function(x, ...) {
  unclass(x)
}

vec_restore.wk_wkt <- function(x, ...) {
  new_wk_wkt(x)
}

vec_cast.wk_wkt <- function(x, to, ...) {
  as_wkt(x)
}

# nocov start
.onLoad <- function(...) {
  # register vctrs dependencies
  for (cls in c("wk_wkb", "wk_wkt")) {
    register_s3_method("vctrs", "vec_proxy", cls)
    register_s3_method("vctrs", "vec_restore", cls)
    register_s3_method("vctrs", "vec_cast", cls)
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
