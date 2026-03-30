#' Convert a distribution or empirical data to a discrete PMF
#'
#' @description Converts either a `distributional` object or a pair of
#'   date/numeric vectors to a `discrete_pmf` object by CDF-differencing
#'   with quantile-based truncation and normalisation.
#'
#'   When `x` is a `distributional` object, the PMF is derived analytically
#'   from the CDF. When `x` and `y` are date or numeric vectors, the PMF is
#'   derived empirically from the observed delays between paired dates.
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued.
#'
#' @param x either a `distributional` object, or a date/numeric vector of
#'   earlier event times (e.g. symptom onset) when used with `y`.
#' @param y a date/numeric vector of later event times (e.g. notification),
#'   paired with `x`. Only used when `x` is a date/numeric vector. Defaults
#'   to `NULL`.
#' @param min_step optional single non-negative integer specifying the minimum
#'   step value. If `NULL`, derived from `min_step_quantile`. If supplied,
#'   `min_step_quantile` is ignored.
#' @param max_step optional single non-negative integer specifying the maximum
#'   step value. If `NULL`, derived from `max_step_quantile`. If supplied,
#'   `max_step_quantile` is ignored.
#' @param min_step_quantile quantile used to derive the minimum step if
#'   `min_step` is not supplied. Defaults to 0.
#' @param max_step_quantile quantile used to derive the maximum step if
#'   `max_step` is not supplied. Defaults to 0.99.
#'
#' @return a `discrete_pmf` object
#'
#' @importFrom distributional cdf
#' @importFrom stats ecdf quantile
#'
#' @export
as_discrete_pmf <- function(x,
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

  } else if (inherits(x, "Date") || is.numeric(x)) {
    validate_xy_vectors(x, y)
    if (!inherits(y, class(x))) {
      cli::cli_abort("`x` and `y` must be the same type.")
    }
    day_diff <- y - x
    cdf_fun <- stats::ecdf(day_diff)
    bounds <- resolve_step_bounds(
      cdf_fun, min_step, max_step,
      min_step_quantile, max_step_quantile
    )

  } else {
    cli::cli_abort(
      "`x` must be a {.cls distribution} object or a date/numeric vector."
    )
  }

  discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)
}
