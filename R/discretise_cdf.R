#' Discretise a CDF into a discrete PMF or curve
#'
#' @description Constructs a two-column data frame with `step` denoting the
#'   discrete index and `mass` denoting the probability of the value falling
#'   between that step and the one before. For example, if `step = 3` and
#'   `mass = 0.25`, there is a 0.25 probability of the value falling between
#'   2 and 3 steps.
#'
#'   When `normalise = TRUE`, masses are divided by their sum, returning a
#'   `discrete_pmf` that sums to 1. When `normalise = FALSE`, masses are
#'   returned as CDF differences without normalisation, returning a
#'   `discrete_curve`.
#'
#' @param min_step a single non-negative integer giving the minimum step value
#' @param max_step a single non-negative integer giving the maximum step value,
#'   must be greater than `min_step`
#' @param cdf_fun a function returning CDF values for given step values
#' @param normalise logical; if `TRUE` masses are normalised to sum to 1.
#'   Defaults to `TRUE`.
#'
#' @return a `discrete_pmf` if `normalise = TRUE`, or a `discrete_curve` if
#'   `normalise = FALSE`
#'
#' @noRd
discretise_cdf <- function(min_step,
                           max_step,
                           cdf_fun,
                           normalise = TRUE) {

  min_step <- validate_bound(min_step, "min_step")
  max_step <- validate_bound(max_step, "max_step")

  if (min_step >= max_step) {
    cli::cli_abort(c(
      "`min_step` must be less than `max_step`.",
      "i" = "`min_step` is {min_step}.",
      "i" = "`max_step` is {max_step}."
    ))
  }

  if (!is.function(cdf_fun)) {
    cli::cli_abort("`cdf_fun` must be a function.")
  }

  if (!is.logical(normalise) || length(normalise) != 1) {
    cli::cli_abort("`normalise` must be a single logical value.")
  }

  delay_massfun <- data.frame(
    step = seq(min_step, max_step)
  ) |>
    dplyr::mutate(
      upper = cdf_fun(step),
      lower = cdf_fun(step - 1),
      mass = upper - lower
    )

  if (normalise) {
    delay_massfun <- delay_massfun |>
      dplyr::mutate(mass = mass / sum(mass))
  }

  delay_massfun <- delay_massfun |>
    dplyr::select(step, mass)

  if (normalise) {
    class(delay_massfun) <- c("discrete_pmf", class(delay_massfun))
  } else {
    class(delay_massfun) <- c("discrete_curve", class(delay_massfun))
  }

  delay_massfun
}

#' @noRd
validate_bound <- function(x, name) {
  if (!is.numeric(x) || length(x) != 1) {
    cli::cli_abort("{.arg {name}} must be a single numeric value.")
  }
  if (is.infinite(x) || is.nan(x)) {
    cli::cli_abort("{.arg {name}} must be a finite value.")
  }
  if (x < 0) {
    cli::cli_abort("{.arg {name}} must be non-negative.")
  }
  if (x != as.integer(x)) {
    cli::cli_warn(c(
      "{.arg {name}} is not an integer.",
      "i" = "Coercing {x} to {as.integer(x)}."
    ))
    x <- as.integer(x)
  }
  invisible(x)
}

#' Plot a discrete_pmf object
#'
#' @param x a `discrete_pmf` object
#' @param y unused, present for compatibility with the generic `plot` method
#' @param ... additional arguments passed to `ggplot2::geom_col()`
#'
#' @return a `ggplot` object
#'
#' @export
plot.discrete_pmf <- function(x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = step, y = mass)) +
    ggplot2::geom_col(width = 0.8, ...) +
    ggplot2::labs(
      x = "Step",
      y = "Probability mass"
    ) +
    ggplot2::theme_minimal()
}

#' Plot a discrete_curve object
#'
#' @param x a `discrete_curve` object
#' @param y unused, present for compatibility with the generic `plot` method
#' @param ... additional arguments passed to `ggplot2::geom_line()`
#'
#' @return a `ggplot` object
#'
#' @export
plot.discrete_curve <- function(x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = step, y = mass)) +
    ggplot2::geom_line(linewidth = 0.8, ...) +
    ggplot2::labs(
      x = "Step",
      y = "Value"
    ) +
    ggplot2::theme_minimal()
}
