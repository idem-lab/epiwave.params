# @noRd
print_discrete_series <- function(x, series_type) {
  n <- length(x$index)
  value_col <- if (series_type == "pmf") "prob" else "weight"
  is_constant <- length(unique(
    vapply(x$values, function(v) paste(v[[value_col]], collapse = ","), character(1))
  )) == 1
  cli::cli_inform(c(
    "A {.cls {class(x)[1]}} object with {n} timepoints",
    "i" = "Index range: {min(x$index)} to {max(x$index)}",
    "i" = "Index class: {.cls {class(x$index)[1]}}",
    "i" = if (is_constant) "Constant {series_type} across all timepoints" else
      "Time-varying {series_type} across timepoints"
  ))
  invisible(x)
}

#' Print a discrete_pmf object
#'
#' @param x a `discrete_pmf` object
#' @param ... unused, present for compatibility with the generic `print` method
#'
#' @return `x` invisibly
#'
#' @export
print.discrete_pmf <- function(x, ...) {
  cli::cli_inform(c(
    "A {.cls discrete_pmf} object with {nrow(x)} steps",
    "i" = "Step range: {min(x$step)} to {max(x$step)}",
    "i" = "Probability sums to: {round(sum(.values(x)), 6)}"
  ))
  print(utils::head(as.data.frame(x), 6))
  if (nrow(x) > 6) {
    cli::cli_inform("... with {nrow(x) - 6} more rows")
  }
  invisible(x)
}

#' Print a discrete_weights object
#'
#' @param x a `discrete_weights` object
#' @param ... unused, present for compatibility with the generic `print` method
#'
#' @return `x` invisibly
#'
#' @export
print.discrete_weights <- function(x, ...) {
  cli::cli_inform(c(
    "A {.cls discrete_weights} object with {nrow(x)} steps",
    "i" = "Step range: {min(x$step)} to {max(x$step)}",
    "i" = "Weight range: {round(min(.values(x)), 6)} to {round(max(.values(x)), 6)}"
  ))
  print(utils::head(as.data.frame(x), 6))
  if (nrow(x) > 6) {
    cli::cli_inform("... with {nrow(x) - 6} more rows")
  }
  invisible(x)
}

#' Print a discrete_pmf_series object
#'
#' @param x a `discrete_pmf_series` object
#' @param ... unused, present for compatibility with the generic `print` method
#'
#' @return `x` invisibly
#'
#' @export
print.discrete_pmf_series <- function(x, ...) {
  print_discrete_series(x, "pmf")
}

#' Print a discrete_weights_series object
#'
#' @param x a `discrete_weights_series` object
#' @param ... unused, present for compatibility with the generic `print` method
#'
#' @return `x` invisibly
#'
#' @export
print.discrete_weights_series <- function(x, ...) {
  print_discrete_series(x, "weights")
}
