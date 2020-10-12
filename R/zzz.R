
# nocov start
.onLoad <- function(...) {

  # Register S3 methods for suggests
  s3_register("sf::st_as_sfc", "wk_wkb")
  s3_register("sf::st_as_sf", "wk_wkb")
  s3_register("sf::st_as_sfc", "wk_wkt")
  s3_register("sf::st_as_sf", "wk_wkt")
  s3_register("sf::st_as_sfc", "wk_wksxp")
  s3_register("sf::st_as_sf", "wk_wksxp")

  for (cls in c("wk_wkb", "wk_wkt", "wk_wksxp", "wk_xy")) {
    s3_register("vctrs::vec_proxy", cls)
    s3_register("vctrs::vec_restore", cls)
    s3_register("vctrs::vec_cast", cls)
    s3_register("vctrs::vec_ptype2", cls)
  }
}

s3_register <- function(generic, class, method = NULL) {
  stopifnot(is.character(generic), length(generic) == 1)
  stopifnot(is.character(class), length(class) == 1)

  pieces <- strsplit(generic, "::")[[1]]
  stopifnot(length(pieces) == 2)
  package <- pieces[[1]]
  generic <- pieces[[2]]

  caller <- parent.frame()

  get_method_env <- function() {
    top <- topenv(caller)
    if (isNamespace(top)) {
      asNamespace(environmentName(top))
    } else {
      caller
    }
  }
  get_method <- function(method, env) {
    if (is.null(method)) {
      get(paste0(generic, ".", class), envir = get_method_env())
    } else {
      method
    }
  }

  method_fn <- get_method(method)
  stopifnot(is.function(method_fn))

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(package, "onLoad"),
    function(...) {
      ns <- asNamespace(package)

      # Refresh the method, it might have been updated by `devtools::load_all()`
      method_fn <- get_method(method)

      registerS3method(generic, class, method_fn, envir = ns)
    }
  )

  # Avoid registration failures during loading (pkgload or regular)
  if (!isNamespaceLoaded(package)) {
    return(invisible())
  }

  envir <- asNamespace(package)

  # Only register if generic can be accessed
  if (exists(generic, envir)) {
    registerS3method(generic, class, method_fn, envir = envir)
  }

  invisible()
}
# nocov end
