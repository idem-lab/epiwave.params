#' Convolve two discrete distribution objects
#'
#' @description Convolves two `discrete_pmf` or `discrete_curve` objects by
#'   discrete summation, returning an object of the same class as the inputs.
#'   Both inputs must be the same class.
#'
#' @param dist1 a `discrete_pmf` or `discrete_curve` object
#' @param dist2 a `discrete_pmf` or `discrete_curve` object
#'
#' @return a `discrete_pmf` or `discrete_curve` object representing the
#'   convolution of `dist1` and `dist2`
#'
#' @export
add_distributions <- function(dist1, dist2) {
  if (!inherits(dist1, c("discrete_pmf", "discrete_curve")) ||
      !inherits(dist2, c("discrete_pmf", "discrete_curve"))) {
    stop("`dist1` and `dist2` must be `discrete_pmf` or `discrete_curve` objects")
  }
  if (class(dist1)[1] != class(dist2)[1]) {
    stop("`dist1` and `dist2` must be the same class")
  }

  dist1 <- dplyr::rename(dist1, delay1 = delays, massfun1 = mass)
  dist2 <- dplyr::rename(dist2, delay2 = delays, massfun2 = mass)

  p <- tidyr::expand_grid(
    delay1 = dist1$delay1,
    delay2 = dist2$delay2) |>
    dplyr::left_join(dist1, by = "delay1") |>
    dplyr::left_join(dist2, by = "delay2") |>
    dplyr::mutate(delay = .data$delay1 + .data$delay2) |>
    dplyr::group_by(.data$delay) |>
    dplyr::summarise(mass = sum(.data$massfun1 * .data$massfun2)) |>
    dplyr::rename(delays = delay)

  class(p) <- c(class(dist1)[1], class(p))
  p
}
