#' Normalise discrete weights to a probability mass function
#'
#' @description Converts a `discrete_weights` object into a
#'   `discrete_pmf` by normalising weights to sum to one.
#'
#' @param x a `discrete_weights` object
#' @param ... unused
#'
#' @return a `discrete_pmf` object
#'
#' @export
normalise <- function(x, ...) {
  UseMethod("normalise")
}

#' @export
normalise.discrete_weights <- function(x, ...) {
  as_discrete_pmf(x)
}

#' @export
normalise.default <- function(x, ...) {
  cli::cli_abort(
    "`normalise()` is only defined for {.cls discrete_weights} objects."
  )
}
