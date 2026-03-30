#' Convolve two discrete objects by discrete summation
#'
#' @description Convolves two `discrete_pmf` or `discrete_curve` objects by
#'   discrete summation, returning an object of the same class as the inputs.
#'   Both inputs must be the same class.
#'
#'   For `discrete_pmf` objects, the result is a new `discrete_pmf`
#'   representing the distribution of the sum of two independent delays.
#'   For `discrete_curve` objects, the result is a new `discrete_curve`
#'   representing the convolution of the two curves.
#'
#' @param dist1 a `discrete_pmf` or `discrete_curve` object
#' @param dist2 a `discrete_pmf` or `discrete_curve` object of the same
#'   class as `dist1`
#'
#' @return a `discrete_pmf` or `discrete_curve` object representing the
#'   convolution of `dist1` and `dist2`
#'
#' @importFrom dplyr rename left_join mutate group_by summarise
#' @importFrom tidyr expand_grid
#'
#' @export
add_discrete <- function(dist1, dist2) {

  if (!inherits(dist1, c("discrete_pmf", "discrete_curve"))) {
    cli::cli_abort(
      "`dist1` must be a {.cls discrete_pmf} or {.cls discrete_curve} object."
    )
  }

  if (!inherits(dist2, c("discrete_pmf", "discrete_curve"))) {
    cli::cli_abort(
      "`dist2` must be a {.cls discrete_pmf} or {.cls discrete_curve} object."
    )
  }

  if (class(dist1)[1] != class(dist2)[1]) {
    cli::cli_abort(c(
      "`dist1` and `dist2` must be the same class.",
      "i" = "`dist1` is {.cls {class(dist1)[1]}}.",
      "i" = "`dist2` is {.cls {class(dist2)[1]}}."
    ))
  }

  out_class <- class(dist1)[1]

  dist1 <- dplyr::rename(dist1, step1 = step, massfun1 = mass)
  dist2 <- dplyr::rename(dist2, step2 = step, massfun2 = mass)

  p <- tidyr::expand_grid(
    step1 = dist1$step1,
    step2 = dist2$step2) |>
    dplyr::left_join(dist1, by = "step1") |>
    dplyr::left_join(dist2, by = "step2") |>
    dplyr::mutate(step = .data$step1 + .data$step2) |>
    dplyr::group_by(.data$step) |>
    dplyr::summarise(mass = sum(.data$massfun1 * .data$massfun2))

  class(p) <- c(out_class, class(p))
  p
}
