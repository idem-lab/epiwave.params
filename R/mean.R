#' Compute the mean of a discrete_pmf object
#'
#' @description Computes the expected value (probability-weighted mean step)
#'   of a `discrete_pmf` object. The mean is defined as the sum of each step
#'   value multiplied by its corresponding probability mass,
#'   \eqn{E[X] = \sum x \cdot P(X = x)}.
#'
#'   This is only defined for `discrete_pmf` objects since `discrete_weights`
#'   objects are not normalised and do not represent a valid probability
#'   distribution.
#'
#' @param x a `discrete_pmf` object
#' @param ... unused, present for compatibility with the generic
#'
#' @return a numeric scalar giving the expected step value
#'
#' @export
mean.discrete_pmf <- function(x, ...) {
  sum(x$step * .values(x))
}

#' Compute the mean of each PMF in a discrete_pmf_series
#'
#' @description Computes the expected value of each `discrete_pmf` in a
#'   `discrete_pmf_series`, returning a numeric vector of means aligned to
#'   the series index.
#'
#' @param x a `discrete_pmf_series` object
#' @param ... unused, present for compatibility with the generic
#'
#' @return a named numeric vector of expected step values, named by the
#'   series index
#'
#' @export
mean.discrete_pmf_series <- function(x, ...) {
  means <- vapply(x$values, mean, numeric(1))
  stats::setNames(means, as.character(x$index))
}
