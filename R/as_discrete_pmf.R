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
#' @param x either a `distributional` object, or a date/numeric vector of
#'   earlier event times (e.g. symptom onset) when used with `y`.
#' @param y a date/numeric vector of later event times (e.g. notification),
#'   paired with `x`. Only used when `x` is a date/numeric vector. Defaults
#'   to `NULL`.
#' @param min_delay_days optional integer specifying the minimum delay in days.
#'   If `NULL`, derived from `min_delay_quantile`.
#' @param max_delay_days optional integer specifying the maximum delay in days.
#'   If `NULL`, derived from `max_delay_quantile`.
#' @param min_delay_quantile quantile used to derive the minimum delay if
#'   `min_delay_days` is not supplied. Defaults to 0.
#' @param max_delay_quantile quantile used to derive the maximum delay if
#'   `max_delay_days` is not supplied. Defaults to 0.99.
#'
#' @return a `discrete_pmf` object
#'
#' @importFrom distributional cdf
#' @importFrom stats ecdf quantile
#'
#' @export
as_discrete_pmf <- function(x,
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

  } else if (inherits(x, "Date") || is.numeric(x)) {
    validate_xy_vectors(x, y)
    if (!inherits(y, class(x))) {
      cli::cli_abort("`x` and `y` must be the same type.")
    }
    day_diff <- y - x
    cdf_fun <- stats::ecdf(day_diff)
    bounds <- resolve_delay_bounds(
      cdf_fun, min_delay_days, max_delay_days,
      min_delay_quantile, max_delay_quantile
    )

  } else {
    cli::cli_abort(
      "`x` must be a {.cls distribution} object or a date/numeric vector."
    )
  }

  discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)
}
