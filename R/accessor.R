#' Internal accessor for discrete measure values
#'
#' Returns the numeric values associated with a discrete_pmf or
#' discrete_weights object - important because the values have unique names.
#' Intended for internal use by operators.
#'
#' @param x A discrete_pmf or discrete_weights object
#' @noRd
.values <- function(x) UseMethod(".values")

#' @noRd
.values.discrete_pmf <- function(x) x$prob

#' @noRd
.values.discrete_weights <- function(x) x$weight

#' @noRd
.values.default <- function(x) {
  cli::cli_abort(
    c(
      "Internal error: unsupported object passed to {.fn .values}.",
      "i" = "Expected a {.cls discrete_pmf} or {.cls discrete_weights} object."
    )
  )
}
