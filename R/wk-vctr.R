
#' @export
print.wk_vctr <- function(x, ...) {
  crs <- wk_crs(x)
  is_geodesic <- wk_is_geodesic(x)
  header <- sprintf("%s[%s]", class(x)[1], length(x))

  if (!is.null(crs)) {
    header <- paste0(header, " with CRS=", wk_crs_format(crs))
  }

  if (isTRUE(is_geodesic)) {
    header <- paste0("geodesic ", header)
  }

  cat(sprintf("<%s>\n", header))

  if (length(x) == 0) {
    return(invisible(x))
  }

  max_print <- getOption("max.print", 1000)
  x_head <- format(utils::head(x, max_print))
  out <- stats::setNames(format(x_head), names(x_head))

  print(out, quote = FALSE)

  if (length(x) > max_print) {
    cat(sprintf("Reached max.print (%s)\n", max_print))
  }

  invisible(x)
}

# lifted from vctrs::obj_leaf()
#' @export
str.wk_vctr <- function(object, ..., indent.str = "", width = getOption("width")) {
  if (length(object) == 0) {
    cat(paste0(" ", class(object)[1], "[0]\n"))
    return(invisible(object))
  }

  # estimate possible number of elements that could be displayed
  # to avoid formatting too many
  width <- width - nchar(indent.str) - 2
  length <- min(length(object), ceiling(width / 5))
  formatted <- format(object[seq_len(length)], trim = TRUE)

  title <- paste0(" ", class(object)[1], "[1:", length(object), "]")
  cat(
    paste0(
      title,
      " ",
      strtrim(paste0(formatted, collapse = ", "), width - nchar(title)),
      "\n"
    )
  )
  invisible(object)
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
`[[<-.wk_vctr` <- function(x, i, value) {
  x[i] <- value
  x
}

#' @export
c.wk_vctr <- function(...) {
  dots <- list(...)
  classes <- lapply(dots, class)
  first_class <- classes[[1]]
  if (!all(vapply(classes, identical, first_class, FUN.VALUE = logical(1)))) {
    stop("Can't combine 'wk_vctr' objects that do not have identical classes.", call. = FALSE)
  }

  # compute output crs, geodesic
  attr(dots[[1]], "crs") <- wk_crs_output(...)
  geodesic <- wk_is_geodesic_output(...)
  attr(dots[[1]], "geodesic") <- if (geodesic) TRUE else NULL

  new_wk_vctr(NextMethod(), dots[[1]])
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

# data.frame() will call as.data.frame() with optional = TRUE
#' @export
as.data.frame.wk_vctr <- function(x, ..., optional = FALSE) {
  if (!optional) {
    stop(sprintf("cannot coerce object of tyoe '%s' to data.frame", class(x)[1]))
  } else {
    new_data_frame(list(x))
  }
}

new_wk_vctr <- function(x, template) {
  structure(
    x,
    class = unique(class(template)),
    crs = attr(template, "crs", exact = TRUE),
    geodesic = attr(template, "geodesic", exact = TRUE)
  )
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
