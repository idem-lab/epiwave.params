#' Convert a distributional object or empirical data to a discrete weights
#'
#' @description Converts either a `distributional` object or paired
#'   step/value vectors to a `discrete_weights` object.
#'
#'   When `x` is a `distributional` object, the weights are derived analytically
#'   from the CDF via CDF-differencing with quantile-based truncation. The
#'   resulting values are not normalised and do not sum to 1.
#'
#'   When `x` and `y` are numeric vectors, the weights are constructed directly
#'   from the supplied values. This is useful when empirical weights are
#'   available from external sources, for example seroprevalence weights
#'   (probability of testing positive by step since infection) or wastewater
#'   shedding weights (viral shedding as a proportion of peak by step since
#'   infection).
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued. Truncation
#'   bounds only apply when `x` is a `distributional` object.
#'
#' @param x either a `distributional` object, or a numeric vector of step
#'   values when used with `y`.
#' @param y a numeric vector of values at each step, paired with `x`. Only
#'   used when `x` is a numeric vector. Defaults to `NULL`.
#' @param min_step optional single non-negative integer specifying the minimum
#'   step value. If `NULL`, derived from `min_step_quantile`. If supplied,
#'   `min_step_quantile` is ignored. Only used when `x` is a `distributional`
#'   object.
#' @param max_step optional single non-negative integer specifying the maximum
#'   step value. If `NULL`, derived from `max_step_quantile`. If supplied,
#'   `max_step_quantile` is ignored. Only used when `x` is a `distributional`
#'   object.
#' @param min_step_quantile quantile used to derive the minimum step if
#'   `min_step` is not supplied. Defaults to 0. Only used when `x` is a
#'   `distributional` object.
#' @param max_step_quantile quantile used to derive the maximum step if
#'   `max_step` is not supplied. Defaults to 0.99. Only used when `x` is a
#'   `distributional` object.
#'
#' @return `discrete_weights` object: a data frame with columns `step` and
#'   `weight` (a non-negative value proportional to the contribution at each
#'   step).
#'
#' @importFrom distributional cdf
#'
#' @export
as_discrete_weights <- function(x,
                              y = NULL,
                              min_step = NULL,
                              max_step = NULL,
                              min_step_quantile = 0.00,
                              max_step_quantile = 0.99) {

  if (inherits(x, "distribution")) {
    cdf_fun <- function(q) distributional::cdf(x, q)[[1]]
    bounds <- resolve_step_bounds(
      x, min_step, max_step,
      min_step_quantile, max_step_quantile
    )
    discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = FALSE)

  } else if (is.numeric(x)) {
    validate_xy_vectors(x, y)
    if (!is.numeric(y)) {
      cli::cli_abort(
        "`y` must be a numeric vector when `x` is a numeric vector."
      )
    }
    weights_df <- data.frame(step = x, mass = y)
    class(weights_df) <- c("discrete_weights", class(weights_df))
    weights_df

  } else {
    cli::cli_abort(
      "`x` must be a {.cls distribution} object or a numeric vector."
    )
  }
}
