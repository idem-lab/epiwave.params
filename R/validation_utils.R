#' @importFrom cli cli_abort cli_warn cli_inform
NULL

# @noRd
validate_series_index <- function(index) {
  if (anyNA(index)) {
    cli::cli_abort("`index` must not contain missing values")
  }
  if (is.unsorted(index, strictly = TRUE)) {
    cli::cli_abort("`index` must be strictly increasing")
  }
  invisible(index)
}

# @noRd
validate_series_list <- function(values, n, expected_class) {
  if (length(values) != n) {
    cli::cli_abort(c(
      "`values` and `index` must have the same length.",
      "i" = "`values` has length {length(values)}.",
      "i" = "`index` has length {n}."
    ))
  }
  if (!all(vapply(values, inherits, logical(1), expected_class))) {
    cli::cli_abort(
      "All elements of `values` must be {.cls {expected_class}} objects."
    )
  }
  invisible(values)
}

#' @noRd
resolve_step_bounds <- function(x,
                                min_step,
                                max_step,
                                min_step_quantile,
                                max_step_quantile) {
  if (!is.null(min_step) && min_step_quantile != 0.00) {
    cli::cli_warn(
      "`min_step_quantile` is ignored when `min_step` is supplied."
    )
  }
  if (!is.null(max_step) && max_step_quantile != 0.99) {
    cli::cli_warn(
      "`max_step_quantile` is ignored when `max_step` is supplied."
    )
  }
  if (is.null(min_step)) {
    min_step <- floor(quantile(x, min_step_quantile))
  }
  if (is.null(max_step)) {
    max_step <- ceiling(quantile(x, max_step_quantile))
  }
  list(min = min_step, max = max_step)
}
