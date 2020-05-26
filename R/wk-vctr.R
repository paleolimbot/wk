
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
  validator <- get(
    paste0("validate_", class(..1)[1]),
    mode = "function",
    envir = asNamespace("wk")
  )
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

parse_base <- function(x, problems) {
  x[!is.na(problems)] <- x[NA_integer_]
  problems_df <- action_for_problems(
    problems,
    function(msg) warning(paste0(msg, '\nSee attr(, "problems") for details.'), call. = FALSE)
  )

  if (nrow(problems_df) > 0) {
    problems_df$actual <- unclass(x)[problems_df$row]
    attr(x, "problems") <- problems_df
  }

  x
}

stop_for_problems <- function(problems)  {
  action_for_problems(problems, stop, call. = FALSE)
}

action_for_problems <- function(problems, action, ...) {
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

    action(
      sprintf(
        "Encountered %s parse problem%s:\n%s",
        n_problems,
        if (n_problems == 1) "" else "s",
        problem_summary
      ),
      ...
    )
  }

  data.frame(
    row = which(!is.na(problems)),
    col = rep_len(NA_integer_, sum(!is.na(problems))),
    expected = problems[!is.na(problems)],
    stringsAsFactors = FALSE
  )
}
