#' Create epiwave_massfun object, which is a two column data frame with delays
#' denoting the number of discrete time steps (e.g., days) of a delay length,
#' and mass denoting the probability of a delay having length between that time
#' step, and the time step before. e.g., if delays = 3 and mass = 0.25, it means
#' there is a 0.25 probability of the delay having a length between 2 and 3
#' days.
#'
#' @param min_delay_days miniumum delay
#' @param max_delay_days maxiumum delay
#' @param cdf_fun function for creating the cdf that defines the mass
#' @param normalise T/F whether the mass should be normalised
#'
#' @return discrete_pmf or discrete_curve object
#' @noRd
discretise_cdf <- function (min_delay_days,
                                    max_delay_days,
                                    cdf_fun,
                                    normalise = TRUE) {

  delay_massfun <- data.frame(
    delays = seq(min_delay_days, max_delay_days)
  ) |>
    dplyr::mutate(
      upper = cdf_fun(delays),
      lower = cdf_fun(delays - 1),
      mass = upper - lower
    )

  if (normalise) {
    delay_massfun <- delay_massfun |>
      dplyr::mutate(mass = mass / sum(mass))
  }

  delay_massfun <- delay_massfun |>
    dplyr::select(
      delays, mass
    )

  if (normalise) {
    class(delay_massfun) <- c("discrete_pmf", class(delay_massfun))
  } else {
    class(delay_massfun) <- c("discrete_curve", class(delay_massfun))
  }

  delay_massfun

}

#' Default plot function for discrete_pmf object
#'
#' @param x x value
#' @param y y value
#' @param ... additional arguments
#'
#' @return plot
#'
#' @export
plot.discrete_pmf <- function (x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = delays, y = mass)) +
    ggplot2::geom_col(width = 0.8) +
    ggplot2::labs(
      x = "Delay (days)",
      y = "Probability mass"
    ) +
    ggplot2::theme_minimal()
}

#' Default plot function for discrete_curve object
#'
#' @param x x value
#' @param y y value
#' @param ... additional arguments
#'
#' @return plot
#'
#' @export
plot.discrete_curve <- function (x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = delays, y = mass)) +
    ggplot2::geom_line(linewidth = 0.8)
    ggplot2::labs(
      x = "Delay (days)",
      y = "Probability mass"
    ) +
    ggplot2::theme_minimal()
}
