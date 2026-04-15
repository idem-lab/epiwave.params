#' Convert a distribution, empirical data, or discrete weights to a discrete PMF
#'
#' @description Converts a `distributional` object, a numeric vector of
#'   observed values, or a `discrete_weights` object to a `discrete_pmf`
#'   object.
#'
#'   When `x` is a `distributional` object, the PMF is derived analytically
#'   from the CDF via CDF-differencing with quantile-based truncation and
#'   normalisation.
#'
#'   When `x` is a numeric vector, the PMF is derived empirically from the
#'   observed values using an empirical CDF.
#'
#'   When `x` is a `discrete_weights` object, the PMF is derived by
#'   normalising the weight values to sum to 1. This is only meaningful if
#'   all weight values are non-negative.
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued. Truncation
#'   bounds only apply when `x` is a `distributional` object or numeric
#'   vector.
#'
#' @param x a `distributional` object, a numeric vector of observed values
#'   (e.g. computed delays) from which an empirical PMF is derived, or a
#'   `discrete_weights` object to be normalised.
#' @param min_step optional single non-negative integer specifying the minimum
#'   step value. If `NULL`, derived from `min_step_quantile`. If supplied,
#'   `min_step_quantile` is ignored. Not used when `x` is a
#'   `discrete_weights` object.
#' @param max_step optional single non-negative integer specifying the maximum
#'   step value. If `NULL`, derived from `max_step_quantile`. If supplied,
#'   `max_step_quantile` is ignored. Not used when `x` is a
#'   `discrete_weights` object.
#' @param min_step_quantile quantile used to derive the minimum step if
#'   `min_step` is not supplied. Defaults to 0. Not used when `x` is a
#'   `discrete_weights` object.
#' @param max_step_quantile quantile used to derive the maximum step if
#'   `max_step` is not supplied. Defaults to 0.99. Not used when `x` is a
#'   `discrete_weights` object.
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
  df <- discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)

  df$prob <- df$mass
  df$mass <- NULL

  class(df) <- c("discrete_pmf", "data.frame")
  df
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
  df <- discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = TRUE)

  df$prob <- df$mass
  df$mass <- NULL

  class(df) <- c("discrete_pmf", "data.frame")
  df
}

#' @rdname as_discrete_pmf
#' @export
as_discrete_pmf.default <- function(x, ...) {
  cli::cli_abort(
    "`x` must be a {.cls distribution} object, a numeric vector, or a {.cls discrete_weights} object."
  )
}

#' @rdname as_discrete_pmf
#' @export
as_discrete_pmf.discrete_weights <- function(x, ...) {

  if (any(x$weight < 0)) {
    cli::cli_abort(
      "Cannot convert to {.cls discrete_pmf}: `weight` contains negative values."
    )
  }

  total <- sum(x$weight)

  if (total == 0) {
    cli::cli_abort(
      "Cannot convert to {.cls discrete_pmf}: `weight` values sum to zero."
    )
  }

  x$prob <- x$weight / total
  x$weight <- NULL

  class(x) <- c("discrete_pmf", class(x)[class(x) != "discrete_weights"])
  x
}

#' Convert a distributional object or empirical data to discrete weights
#'
#' @description Converts a `distributional` object or empirical step/value
#'   data to a `discrete_weights` object.
#'
#'   When `x` is a `distributional` object, the weights are derived
#'   analytically from the CDF via CDF-differencing with quantile-based
#'   truncation. The resulting values are not normalised and do not sum to 1.
#'
#'   When `x` is a numeric vector of step values and `y` is a numeric vector
#'   of corresponding weight values, the weights are constructed directly from
#'   the supplied values. This is useful when empirical weights are available
#'   from external sources, for example seroprevalence curves (probability of
#'   testing positive by step since infection) or wastewater shedding curves
#'   (viral shedding as a proportion of peak by step since infection).
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued. Truncation
#'   bounds only apply when `x` is a `distributional` object.
#'
#' @param x a `distributional` object, or a numeric vector of step values
#'   when used with `y`.
#' @param y a numeric vector of weight values at each step, paired with `x`.
#'   Only used when `x` is a numeric vector. Defaults to `NULL`.
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
#' @param ... additional arguments passed to methods
#'
#' @return a `discrete_weights` object: a data frame with columns `step` and
#'   `mass` (a non-negative value proportional to the contribution at each
#'   step).
#'
#' @export
as_discrete_weights <- function(x, ...) {
  UseMethod("as_discrete_weights")
}

#' @rdname as_discrete_weights
#' @importFrom distributional cdf
#' @importFrom stats quantile
#' @export
as_discrete_weights.distribution <- function(x,
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
  df <- discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = FALSE)

  df$weight <- df$mass
  df$mass <- NULL

  class(df) <- c("discrete_weights", "data.frame")
  df
}

#' @rdname as_discrete_weights
#' @export
as_discrete_weights.numeric <- function(x,
                                        y = NULL,
                                        ...) {
  if (is.null(y)) {
    cli::cli_abort(
      "`y` must be supplied when `x` is a numeric vector."
    )
  }
  if (!is.numeric(y)) {
    cli::cli_abort("`y` must be a numeric vector.")
  }
  if (length(x) != length(y)) {
    cli::cli_abort(c(
      "`x` and `y` must be the same length.",
      "i" = "`x` has length {length(x)}.",
      "i" = "`y` has length {length(y)}."
    ))
  }
  if (any(y < 0)) {
    cli::cli_abort(
      "`y` must not contain negative values."
    )
  }
  df <- data.frame(step = x, weight = y)

  class(df) <- c("discrete_weights", "data.frame")
  df
}

#' @rdname as_discrete_weights
#' @export
as_discrete_weights.default <- function(x, ...) {
  cli::cli_abort(
    "`x` must be a {.cls distribution} object or a numeric vector."
  )
}

#' Convert a distributional object or empirical data to discrete weights
#'
#' @description Converts a `distributional` object or empirical step/value
#'   data to a `discrete_weights` object.
#'
#'   When `x` is a `distributional` object, the weights are derived
#'   analytically from the CDF via CDF-differencing with quantile-based
#'   truncation. The resulting values are not normalised and do not sum to 1.
#'
#'   When `x` is a numeric vector of step values and `y` is a numeric vector
#'   of corresponding weight values, the weights are constructed directly from
#'   the supplied values. This is useful when empirical weights are available
#'   from external sources, for example seroprevalence curves (probability of
#'   testing positive by step since infection) or wastewater shedding curves
#'   (viral shedding as a proportion of peak by step since infection).
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued. Truncation
#'   bounds only apply when `x` is a `distributional` object.
#'
#' @param x a `distributional` object, or a numeric vector of step values
#'   when used with `y`.
#' @param y a numeric vector of weight values at each step, paired with `x`.
#'   Only used when `x` is a numeric vector. Defaults to `NULL`.
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
#' @param ... additional arguments passed to methods
#'
#' @return a `discrete_weights` object: a data frame with columns `step` and
#'   `mass` (a non-negative value proportional to the contribution at each
#'   step).
#'
#' @export
as_discrete_weights <- function(x, ...) {
  UseMethod("as_discrete_weights")
}

#' @rdname as_discrete_weights
#' @importFrom distributional cdf
#' @importFrom stats quantile
#' @export
as_discrete_weights.distribution <- function(x,
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
  df <- discretise_cdf(bounds$min, bounds$max, cdf_fun, normalise = FALSE)

  df$weight <- df$mass
  df$mass <- NULL

  class(df) <- c("discrete_weights", "data.frame")
  df
}

#' @rdname as_discrete_weights
#' @export
as_discrete_weights.numeric <- function(x,
                                        y = NULL,
                                        ...) {
  if (is.null(y)) {
    cli::cli_abort(
      "`y` must be supplied when `x` is a numeric vector."
    )
  }
  if (!is.numeric(y)) {
    cli::cli_abort("`y` must be a numeric vector.")
  }
  if (length(x) != length(y)) {
    cli::cli_abort(c(
      "`x` and `y` must be the same length.",
      "i" = "`x` has length {length(x)}.",
      "i" = "`y` has length {length(y)}."
    ))
  }
  if (any(y < 0)) {
    cli::cli_abort(
      "`y` must not contain negative values."
    )
  }
  df <- data.frame(step = x, weight = y)

  class(df) <- c("discrete_weights", "data.frame")
  df
}

#' @rdname as_discrete_weights
#' @export
as_discrete_weights.default <- function(x, ...) {
  cli::cli_abort(
    "`x` must be a {.cls distribution} object or a numeric vector."
  )
}
