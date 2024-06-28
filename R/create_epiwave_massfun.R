#' Create epiwave_massfun object, which is a two column data frame with delays
#' denoting the number of discrete time steps (e.g., days) of a delay length,
#' and mass denoting the probability of a delay having length between that time
#' step, and the time step before. e.g., if delays = 3 and mass = 0.25, it means
#' there is a 0.25 probability of the delay having a length between 2 and 3
#' days.
#'
#' @param min_delay miniumum delay
#' @param max_delay maxiumum delay
#' @param cdf_fun function for creating the cdf that defines the mass
#' @param normalise whether the mass should be normalised
#'
#' @return epiwave_massfun object
#' @export
create_epiwave_massfun <- function (min_delay_days,
                                    max_delay_days,
                                    cdf_fun,
                                    normalise = c(TRUE, FALSE)) {

  delay_massfun <- data.frame(
    delays = seq(min_delay_days, max_delay_days)
  ) %>%
    dplyr::mutate(
      upper = cdf_fun(delays),
      lower = cdf_fun(delays - 1),
      mass = upper - lower
    )

  if (normalise) {
    delay_massfun <- delay_massfun %>%
      dplyr::mutate(
        correction = sum(mass),
        mass = mass / correction
      )
  }

  delay_massfun <- delay_massfun %>%
    dplyr::select(
      delays, mass
    )

  class(delay_massfun) <- c("epiwave_massfun", class(delay_massfun))

  if (normalise) {
    class(delay_massfun) <- c("epiwave_distribution_massfun", class(delay_massfun))
  }
  if (!normalise) {
    class(delay_massfun) <- c("epiwave_curve_massfun", class(delay_massfun))
  }

  delay_massfun

}

#' Default plot function for epiwave_massfun object
#'
#' @param x x value
#' @param y y value
#' @param ... additional arguments
#'
#' @return plot
#'
#' @export
plot.epiwave_massfun <- function (x, y, ...) {
  barplot(x$mass, width = 1, names.arg = x$delays,
          xlab = "delay (days)",
          ylab = "probability")
}
