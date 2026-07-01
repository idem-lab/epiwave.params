#' Convert a distribution, empirical data, or discrete weights to a discrete PMF
#'
#' @description Converts a `distributional` object, a numeric vector of
#'   observed values, an `ecdf` object, a `difftime` object, or a
#'   `discrete_weights` object to a `discrete_pmf` object.
#'
#'   When `x` is a `distributional` object, the PMF is derived analytically
#'   from the CDF via CDF-differencing with quantile-based truncation and
#'   normalisation.
#'
#'   When `x` is a numeric vector, the PMF is derived empirically from the
#'   observed values using an empirical CDF.
#'
#'   When `x` is an `ecdf` object, the supplied empirical CDF is used directly
#'   without reconstruction.
#'
#'   When `x` is a `difftime` object, it is converted to numeric using its
#'   recorded units and then treated as the numeric case.
#'
#'   When `x` is a `discrete_weights` object, the PMF is derived by
#'   normalising the weight values to sum to 1. This is only meaningful if
#'   all weight values are non-negative.
#'
#'   The truncation bounds are set either explicitly via `min_step` and
#'   `max_step`, or derived from quantiles of the distribution via
#'   `min_step_quantile` and `max_step_quantile`. If both are supplied,
#'   the explicit bounds take precedence and a warning is issued. Truncation
#'   bounds only apply when `x` is a `distributional`, numeric, `ecdf`, or
#'   `difftime` object.
#'
#' @param x a `distributional` object, a numeric vector of observed values
#'   (e.g. computed delays) from which an empirical PMF is derived, an `ecdf`
#'   object, a `difftime` object, or a `discrete_weights` object to be
#'   normalised.
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

  if (length(x) == 0L) {
    cli::cli_abort("`x` must contain at least one value.")
  }

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
#' @importFrom stats quantile
#' @export
as_discrete_pmf.ecdf <- function(x,
                                 min_step = NULL,
                                 max_step = NULL,
                                 min_step_quantile = 0.00,
                                 max_step_quantile = 0.99,
                                 ...) {
  bounds <- resolve_step_bounds(
    x, min_step, max_step,
    min_step_quantile, max_step_quantile
  )
  df <- discretise_cdf(bounds$min, bounds$max, x, normalise = TRUE)

  df$prob <- df$mass
  df$mass <- NULL

  class(df) <- c("discrete_pmf", "data.frame")
  df
}

#' @rdname as_discrete_pmf
#' @export
as_discrete_pmf.difftime <- function(x,
                                     min_step = NULL,
                                     max_step = NULL,
                                     min_step_quantile = 0.00,
                                     max_step_quantile = 0.99,
                                     ...) {

  unit <- attr(x, "units")
  if (is.null(unit)) {
    cli::cli_abort("`difftime` object has no units attribute.")
  }

  cli::cli_inform(c(
    "i" = "Converting {.cls difftime} to numeric using unit: {.val {unit}}."
  ))

  x_num <- as.numeric(x)

  as_discrete_pmf.numeric(
    x_num,
    min_step = min_step,
    max_step = max_step,
    min_step_quantile = min_step_quantile,
    max_step_quantile = max_step_quantile,
    ...
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

#' @rdname as_discrete_pmf
#' @export
as_discrete_pmf.default <- function(x, ...) {
  cli::cli_abort(
    "`x` must be a {.cls distribution} object, a numeric vector, an {.cls ecdf} object, a {.cls difftime} object, or a {.cls discrete_weights} object."
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
#'   `weight` (a non-negative value proportional to the contribution at each
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
  if (length(x) == 0L) {
    cli::cli_abort("`x` must contain at least one value.")
  }

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

#' Discretise a CDF into a discrete PMF or weight function
#'
#' @description
#' Constructs a two-column data frame with `step` denoting the discrete index
#' and `mass` denoting the probability of the value falling between that step
#' and the one before. For example, if `step = 3` and `mass = 0.25`, there is
#' a 0.25 probability of the value falling between 2 and 3 steps.
#'
#' When `normalise = TRUE`, masses are divided by their sum, returning a
#' `discrete_pmf` that sums to 1. When `normalise = FALSE`, masses are returned
#' as CDF differences without normalisation, returning a `discrete_weights`.
#'
#' @param min_step A single non-negative integer giving the minimum step value.
#' @param max_step A single non-negative integer giving the maximum step value;
#'   must be greater than `min_step`.
#' @param cdf_fun A function returning CDF values for given step values.
#' @param normalise Logical; if `TRUE`, masses are normalised to sum to 1.
#'   Defaults to `TRUE`.
#'
#' @importFrom rlang .data
#'
#' @return
#' A `discrete_pmf` if `normalise = TRUE`, or a `discrete_weights` if
#' `normalise = FALSE`.
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
  delay_massfun <-
    data.frame(step = seq(min_step, max_step)) |>
    dplyr::mutate(
      upper = cdf_fun(.data$step),
      lower = cdf_fun(.data$step - 1),
      mass  = .data$upper - .data$lower
    )
  if (normalise) {
    delay_massfun <-
      delay_massfun |>
      dplyr::mutate(mass = .data$mass / sum(.data$mass))
  }
  delay_massfun <-
    delay_massfun |>
    dplyr::select("step", "mass")
  if (normalise) {
    class(delay_massfun) <- c("discrete_pmf", class(delay_massfun))
  } else {
    class(delay_massfun) <- c("discrete_weights", class(delay_massfun))
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
