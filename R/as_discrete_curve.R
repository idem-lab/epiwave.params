#' Convert a distributional object or empirical data to a discrete curve
#'
#' @description Converts either a `distributional` object or paired
#'   day/value vectors to a `discrete_curve` object.
#'
#'   When `x` is a `distributional` object, the curve is derived analytically
#'   from the CDF via CDF-differencing with quantile-based truncation. The
#'   resulting values are not normalised and do not sum to 1.
#'
#'   When `x` and `y` are numeric vectors, the curve is constructed directly
#'   from the supplied values. This is useful when empirical curves are
#'   available from external sources, for example seroprevalence curves
#'   (probability of testing positive by day since infection) or wastewater
#'   shedding curves (viral shedding as a proportion of peak by day since
#'   infection).
#'
#' @param x either a `distributional` object, or a numeric vector of days
#'   since infection onset when used with `y`.
#' @param y a numeric vector of values at each day, paired with `x`. Only
#'   used when `x` is a numeric vector. Defaults to `NULL`.
#' @param min_delay_days optional integer specifying the minimum delay in days.
#'   If `NULL`, derived from `min_delay_quantile`. Only used when `x` is a
#'   `distributional` object.
#' @param max_delay_days optional integer specifying the maximum delay in days.
#'   If `NULL`, derived from `max_delay_quantile`. Only used when `x` is a
#'   `distributional` object.
#' @param min_delay_quantile quantile used to derive the minimum delay if
#'   `min_delay_days` is not supplied. Defaults to 0. Only used when `x` is a
#'   `distributional` object.
#' @param max_delay_quantile quantile used to derive the maximum delay if
#'   `max_delay_days` is not supplied. Defaults to 0.99. Only used when `x`
#'   is a `distributional` object.
#'
#' @return a `discrete_curve` object
#'
#' @importFrom distributional cdf
#'
#' @export
as_discrete_curve <- function(x,
                              y = NULL,
                              min_delay_days = NULL,
                              max_delay_days = NULL,
                              min_delay_quantile = 0.00,
                              max_delay_quantile = 0.99) {

  if (inherits(x, "distribution")) {
    cdf_fun <- function(q) distributional::cdf(x, q)[[1]]
    bounds <- resolve_delay_bounds(
      x, min_delay_days, max_delay_days,
      min_delay_quantile, max_delay_quantile
    )
    discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = FALSE)

  } else if (is.numeric(x)) {
    validate_xy_vectors(x, y)
    if (!is.numeric(y)) {
      cli::cli_abort(
        "`y` must be a numeric vector when `x` is a numeric vector."
      )
    }
    curve_df <- data.frame(delays = x, mass = y)
    class(curve_df) <- c("discrete_curve", class(curve_df))
    curve_df

  } else {
    cli::cli_abort(
      "`x` must be a {.cls distribution} object or a numeric vector."
    )
  }
}
