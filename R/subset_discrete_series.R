#' Subset a discrete_pmf_series
#'
#' @description Subsets a `discrete_pmf_series` object by position, logical
#'   vector, or index value, returning a new `discrete_pmf_series` with the
#'   selected elements. Class and index alignment are preserved.
#'
#' @param x a `discrete_pmf_series` object
#' @param i integer positions, a logical vector of the same length as the
#'   series, or a vector of index values matching the type of `x$index`
#' @param ... unused, present for compatibility with the generic
#'
#' @return a `discrete_pmf_series` object
#'
#' @export
"[.discrete_pmf_series" <- function(x, i, ...) {
  subset_discrete_series(x, i)
}

#' Subset a discrete_weights_series
#'
#' @description Subsets a `discrete_weights_series` object by position, logical
#'   vector, or index value, returning a new `discrete_weights_series` with the
#'   selected elements. Class and index alignment are preserved.
#'
#' @param x a `discrete_weights_series` object
#' @param i integer positions, a logical vector of the same length as the
#'   series, or a vector of index values matching the type of `x$index`
#' @param ... unused, present for compatibility with the generic
#'
#' @return a `discrete_weights_series` object
#'
#' @export
"[.discrete_weights_series" <- function(x, i, ...) {
  subset_discrete_series(x, i)
}

#' @noRd
subset_discrete_series <- function(x, i) {
  if (inherits(i, class(x$index))) {
    i <- match(i, x$index)
    if (anyNA(i)) {
      cli::cli_abort("Some index values were not found in the series.")
    }
  }
  structure(
    list(
      index = x$index[i],
      values = x$values[i]
    ),
    class = class(x)
  )
}
