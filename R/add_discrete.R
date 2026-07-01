#' Convolve two discrete objects by discrete summation
#'
#' @description Convolves two `discrete_pmf` or `discrete_weights` objects by
#'   discrete summation, returning an object of the same class as the inputs.
#'   Both inputs must be the same class. This function assumes input objects
#'   satisfy the invariants of their class (e.g., probability mass functions
#'   are normalised). These invariants are enforced during construction.
#'
#' @param discrete1 a `discrete_pmf` or `discrete_weights` object
#' @param discrete2 a `discrete_pmf` or `discrete_weights` object of the same
#'   class as `discrete1`
#'
#' @return For `discrete_pmf` objects, the result is a new `discrete_pmf`
#'   representing the distribution of the sum of two independent delays.
#'   For `discrete_weights` objects, the result is a new `discrete_weights`
#'   representing the convolution of the two weights.
#'
#' @importFrom dplyr rename left_join mutate group_by summarise
#' @importFrom tidyr expand_grid
#' @importFrom rlang .data
#'
#' @export
add_discrete <- function(discrete1, discrete2) {

  if (!inherits(discrete1, c("discrete_pmf", "discrete_weights"))) {
    cli::cli_abort(
      "`discrete1` must be a {.cls discrete_pmf} or {.cls discrete_weights} object."
    )
  }

  if (!inherits(discrete2, c("discrete_pmf", "discrete_weights"))) {
    cli::cli_abort(
      "`discrete2` must be a {.cls discrete_pmf} or {.cls discrete_weights} object."
    )
  }

  if (nrow(discrete1) == 0 || nrow(discrete2) == 0) {
    cli::cli_abort("Cannot convolve empty discrete objects.")
  }

  cls <- class(discrete1)[1]

  if (cls != class(discrete2)[1]) {
    cli::cli_abort(c(
      "`discrete1` and `discrete2` must be the same class.",
      "i" = "`discrete1` is {.cls {cls}}.",
      "i" = "`discrete2` is {.cls {class(discrete2)[1]}}."
    ))
  }

  d1 <- dplyr::mutate(
    discrete1,
    value1 = .values(discrete1)
  )

  d2 <- dplyr::mutate(
    discrete2,
    value2 = .values(discrete2)
  )

  p <- tidyr::expand_grid(
    step1 = d1$step,
    step2 = d2$step) |>
    dplyr::left_join(d1, by = c("step1" = "step")) |>
    dplyr::left_join(d2, by = c("step2" = "step")) |>
    dplyr::mutate(
      step = .data$step1 + .data$step2,
      value = .data$value1 * .data$value2) |>
    dplyr::group_by(.data$step) |>
    dplyr::summarise(value = sum(.data$value), .groups = "drop")

  # restore semantic column name
  if (cls == "discrete_pmf") {
    p$prob <- p$value
    p$value <- NULL
  } else {
    p$weight <- p$value
    p$value <- NULL
  }

  class(p) <- c(cls, "data.frame")
  p
}

#' Add two discrete objects using the + operator
#'
#' @description Convolves two `discrete_pmf` or `discrete_weights` objects using
#'   the `+` operator. Both objects must be the same class. This is equivalent
#'   to calling `add_discrete(discrete1, discrete2)`.
#'
#'   Note that `+` here means convolution of the two distributions, not
#'   element-wise addition of masses.
#'
#' @param e1 a `discrete_pmf` or `discrete_weights` object
#' @param e2 a `discrete_pmf` or `discrete_weights` object of the same class
#'   as `e1`
#'
#' @return For `discrete_pmf` objects, the result is a new `discrete_pmf`
#'   representing the distribution of the sum of two independent delays.
#'   For `discrete_weights` objects, the result is a new `discrete_weights`
#'   representing the convolution of the two weights.
#'
#' @name plus-discrete
#' @export
"+.discrete_pmf" <- function(e1, e2) {
  add_discrete(e1, e2)
}

#' @rdname plus-discrete
#' @export
"+.discrete_weights" <- function(e1, e2) {
  add_discrete(e1, e2)
}
