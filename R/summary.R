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
