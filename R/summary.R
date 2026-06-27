#' Summarise a discrete_pmf object
#'
#' @param object a `discrete_pmf` object
#' @param ... unused, present for compatibility with the generic `summary`
#'   method
#'
#' @return a named numeric vector of summary statistics, invisibly
#'
#' @export
summary.discrete_pmf <- function(object, ...) {
  pmf_mean <- mean(object)
  pmf_var <- var(object)
  stats <- c(
    n_steps    = nrow(object),
    min_step   = min(object$step),
    max_step   = max(object$step),
    prob_sum   = sum(.values(object)),
    modal_step = object$step[which.max(.values(object))],
    mean_step  = pmf_mean,
    var_step   = pmf_var,
    sd_step    = sqrt(pmf_var)
  )
  cli::cli_inform(c(
    "A {.cls discrete_pmf} object",
    "i" = "Steps: {stats['n_steps']} ({stats['min_step']} to {stats['max_step']})",
    "i" = "Probability sum: {round(stats['prob_sum'], 6)}",
    "i" = "Modal step: {stats['modal_step']}",
    "i" = "Mean step: {round(stats['mean_step'], 3)}",
    "i" = "Variance: {round(stats['var_step'], 3)}",
    "i" = "SD: {round(stats['sd_step'], 3)}"
  ))
  invisible(stats)
}

#' Summarise a discrete_weights object
#'
#' @param object a `discrete_weights` object
#' @param ... unused, present for compatibility with the generic `summary`
#'   method
#'
#' @return a named numeric vector of summary statistics, invisibly
#'
#' @export
summary.discrete_weights <- function(object, ...) {
  stats <- c(
    n_steps    = nrow(object),
    min_step   = min(object$step),
    max_step   = max(object$step),
    min_weight = min(.values(object)),
    max_weight = max(.values(object)),
    peak_step  = object$step[which.max(.values(object))]
  )
  cli::cli_inform(c(
    "A {.cls discrete_weights} object",
    "i" = "Steps: {stats['n_steps']} ({stats['min_step']} to {stats['max_step']})",
    "i" = "Weight range: {round(stats['min_weight'], 6)} to {round(stats['max_weight'], 6)}",
    "i" = "Peak step: {stats['peak_step']}"
  ))
  invisible(stats)
}

# @noRd
summarise_series_index <- function(index) {
  if (inherits(index, "Date")) {
    list(
      min = min(index),
      max = max(index),
      as_stats = c(
        min_index = as.numeric(min(index)),
        max_index = as.numeric(max(index))
      ),
      formatted = c(
        min = format(min(index)),
        max = format(max(index))
      )
    )
  } else {
    list(
      min = min(index),
      max = max(index),
      as_stats = c(
        min_index = min(index),
        max_index = max(index)
      ),
      formatted = c(
        min = as.character(min(index)),
        max = as.character(max(index))
      )
    )
  }
}

#' Summarise a discrete_pmf_series object
#'
#' @param object a `discrete_pmf_series` object
#' @param ... unused, present for compatibility with the generic `summary`
#'   method
#'
#' @return a named numeric vector of summary statistics, invisibly
#'
#' @export
summary.discrete_pmf_series <- function(object, ...) {
  n <- length(object$index)
  idx <- summarise_series_index(object$index)
  timepoint_means <- mean(object)
  is_constant <- length(unique(
    vapply(object$values, function(v) paste(v$prob, collapse = ","), character(1))
  )) == 1
  stats <- c(
    n_timepoints   = n,
    idx$as_stats,
    min_mean_step  = min(timepoint_means),
    max_mean_step  = max(timepoint_means),
    mean_mean_step = mean(timepoint_means)
  )
  cli::cli_inform(c(
    "A {.cls discrete_pmf_series} object",
    "i" = "Timepoints: {n} ({idx$formatted['min']} to {idx$formatted['max']})",
    "i" = "Index class: {.cls {class(object$index)[1]}}",
    "i" = "Mean step range: {round(min(timepoint_means), 3)} to {round(max(timepoint_means), 3)}",
    "i" = "Mean of mean steps: {round(mean(timepoint_means), 3)}",
    "i" = if (is_constant) "Constant PMF across all timepoints" else
      "Time-varying PMF across timepoints"
  ))
  invisible(stats)
}

#' Summarise a discrete_weights_series object
#'
#' @param object a `discrete_weights_series` object
#' @param ... unused, present for compatibility with the generic `summary`
#'   method
#'
#' @return a named numeric vector of summary statistics, invisibly
#'
#' @export
summary.discrete_weights_series <- function(object, ...) {
  n <- length(object$index)
  idx <- summarise_series_index(object$index)
  is_constant <- length(unique(
    vapply(object$values, function(v) paste(v$weight, collapse = ","), character(1))
  )) == 1
  stats <- c(
    n_timepoints = n,
    idx$as_stats
  )
  cli::cli_inform(c(
    "A {.cls discrete_weights_series} object",
    "i" = "Timepoints: {n} ({idx$formatted['min']} to {idx$formatted['max']})",
    "i" = "Index class: {.cls {class(object$index)[1]}}",
    "i" = if (is_constant) "Constant weights across all timepoints" else
      "Time-varying weights across timepoints"
  ))
  invisible(stats)
}
