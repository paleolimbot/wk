
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

#' @method rep_len wk_vctr
#' @export
rep_len.wk_vctr <- function(x, ...) {
  new_wk_vctr(NextMethod(), x)
}

new_wk_vctr <- function(x, template) {
  structure(x, class = unique(class(template)))
}

stop_for_problems <- function(problems) {
  if (any(!is.na(problems))) {
    n_problems <- sum(!is.na(problems))
    summary_problems <- utils::head(which(!is.na(problems)))
    problem_summary <- paste0(
      sprintf("[%s] %s", summary_problems, problems[summary_problems]),
      collapse = "\n"
    )

    if (n_problems > length(summary_problems)) {
      problem_summary <- paste0(
        problem_summary,
        sprintf("\n...and %s more problems", n_problems - length(summary_problems))
      )
    }

    stop(
      sprintf(
        "Encountered %s parse problem%s:\n%s",
        n_problems,
        if (n_problems == 1) "" else "s",
        problem_summary
      ),
      call. = FALSE
    )
  }
  invisible(problems)
}
