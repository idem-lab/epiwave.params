#' Convert a distribution or empirical data to a discrete PMF
#'
#' @description Converts either a `distributional` object or a numeric vector
#'   of observed values to a `discrete_pmf` object by CDF-differencing with
#'   quantile-based truncation and normalisation.
#'
#'   When `x` is a `distributional` object, the PMF is derived analytically
#'   from the CDF. When `x` is a numeric vector, the PMF is derived
#'   empirically from the observed values using an empirical CDF.
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued.
#'
#' @param x either a `distributional` object, or a numeric vector of observed
#'   values (e.g. computed delays) from which an empirical PMF is derived.
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
#' @param ... additional arguments passed to methods
#'
#' @return a `discrete_pmf` object
#'
#' @export
as_discrete_pmf <- function(x, ...) {
  UseMethod("as_discrete_pmf")
}

#' @rdname as_discrete_pmf
#' @importFrom distributional cdf
#' @importFrom stats quantile
#' @export
as_discrete_pmf.distribution <- function(x,
                                         min_step = NULL,
                                         max_step = NULL,
                                         min_step_quantile = 0.00,
                                         max_step_quantile = 0.99,
                                         ...) {
  cdf_fun <- function(q) distributional::cdf(x, q)[[1]]
  bounds <- resolve_step_bounds(
    x, min_step, max_step,
    min_step_quantile, max_step_quantile
  )
  discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)
}

#' @rdname as_discrete_pmf
#' @importFrom stats ecdf quantile
#' @export
as_discrete_pmf.numeric <- function(x,
                                    min_step = NULL,
                                    max_step = NULL,
                                    min_step_quantile = 0.00,
                                    max_step_quantile = 0.99,
                                    ...) {
  cdf_fun <- stats::ecdf(x)
  bounds <- resolve_step_bounds(
    cdf_fun, min_step, max_step,
    min_step_quantile, max_step_quantile
  )
  discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)
}

#' @rdname as_discrete_pmf
#' @export
as_discrete_pmf.default <- function(x, ...) {
  cli::cli_abort(
    "`x` must be a {.cls distribution} object or a numeric vector."
  )
}
