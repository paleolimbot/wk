
#' @export
print.wk_vctr <- function(x, ...) {
  cat(sprintf("<%s[%s]>\n", class(x)[1], length(x)))
  if (length(x) == 0) {
    return(invisible(x))
  }

  out <- stats::setNames(format(x), names(x))
  print(out, quote = FALSE)
  invisible(x)
}

#' @export
is.na.wk_vctr <- function(x) {
  vapply(x, is.null, logical(1))
}

#' @export
`[.wk_vctr` <- function(x, i) {
  new_wk_vctr(NextMethod(), x)
}

#' @export
`[[.wk_vctr` <- function(x, i) {
  x[i]
}

#' @export
c.wk_vctr <- function(...) {
  result <- new_wk_vctr(NextMethod(), ..1)
  validator <- match.fun(paste0("validate_", class(..1)[1]))
  validator(result)
  result
}

#' @export
rep.wk_vctr <- function(x, ...) {
  new_wk_vctr(NextMethod(), x)
}

#' @export
rep_len.wk_vctr <- function(x, ...) {
  new_wk_vctr(NextMethod(), x)
}

new_wk_vctr <- function(x, template) {
  structure(x, class = unique(class(template)))
}

stop_for_problems <- function(problems) {
  if (any(!is.na(problems))) {
    problem_summary <- paste0(
      sprintf("[%s] %s", which(!is.na(problems)), problems[!is.na(problems)]),
      collapse = "\n"
    )

    stop(
      sprintf(
        "Encountered %s parse problems:\n%s",
        sum(!is.na(problems)), problem_summary
      )
    )
  }
  invisible(problems)
}
