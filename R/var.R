#' Compute the variance of a discrete_pmf object
#'
#' @description Computes the variance of the discrete random variable
#'   represented by a `discrete_pmf` object, defined as
#'   \eqn{Var(X) = E[X^2] - (E[X])^2}.
#'
#'   This is only defined for `discrete_pmf` objects since `discrete_weights`
#'   objects are not normalised and do not represent a valid probability
#'   distribution.
#'
#' @param x a `discrete_pmf` object
#' @param ... unused, present for compatibility with the generic
#'
#' @return a numeric scalar giving the variance of the step distribution
#'
#' @export
var.discrete_pmf <- function(x, ...) {
  sum(x$step^2 * .values(x)) - mean(x)^2
}
