#' Convolve two discrete objects by discrete summation
#'
#' @description Convolves two `discrete_pmf` or `discrete_weights` objects by
#'   discrete summation, returning an object of the same class as the inputs.
#'   Both inputs must be the same class.
#'
#'   For `discrete_pmf` objects, the result is a new `discrete_pmf`
#'   representing the distribution of the sum of two independent delays.
#'   For `discrete_weights` objects, the result is a new `discrete_weights`
#'   representing the convolution of the two weights.
#'
#' @param discrete1 a `discrete_pmf` or `discrete_weights` object
#' @param discrete2 a `discrete_pmf` or `discrete_weights` object of the same
#'   class as `discrete1`
#'
#' @return a `discrete_pmf` or `discrete_weights` object representing the
#'   convolution of `discrete1` and `discrete2`
#'
#' @importFrom dplyr rename left_join mutate group_by summarise
#' @importFrom tidyr expand_grid
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

  if (class(discrete1)[1] != class(discrete2)[1]) {
    cli::cli_abort(c(
      "`discrete1` and `discrete2` must be the same class.",
      "i" = "`discrete1` is {.cls {class(discrete1)[1]}}.",
      "i" = "`discrete2` is {.cls {class(discrete2)[1]}}."
    ))
  }

  out_class <- class(discrete1)[1]

  discrete1 <- dplyr::rename(discrete1, step1 = step, massfun1 = mass)
  discrete2 <- dplyr::rename(discrete2, step2 = step, massfun2 = mass)

  p <- tidyr::expand_grid(
    step1 = discrete1$step1,
    step2 = discrete2$step2) |>
    dplyr::left_join(discrete1, by = "step1") |>
    dplyr::left_join(discrete2, by = "step2") |>
    dplyr::mutate(step = .data$step1 + .data$step2) |>
    dplyr::group_by(.data$step) |>
    dplyr::summarise(mass = sum(.data$massfun1 * .data$massfun2))

  class(p) <- c(out_class, class(p))
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
#' @return a `discrete_pmf` or `discrete_weights` object representing the
#'   convolution of `e1` and `e2`
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
