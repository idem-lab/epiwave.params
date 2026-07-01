#' Truncate a discrete object to a specified step range
#'
#' @description Truncates a `discrete_pmf` or `discrete_weights` object to
#'   the specified step range. For `discrete_pmf` objects, the truncated
#'   masses are renormalised to sum to 1. For `discrete_weights` objects,
#'   values outside the specified range are dropped without renormalisation.
#'
#' @param x a `discrete_pmf` or `discrete_weights` object
#' @param min_step optional single non-negative integer giving the minimum
#'   step to retain. If `NULL`, the existing minimum step is used.
#' @param max_step optional single non-negative integer giving the maximum
#'   step to retain. If `NULL`, the existing maximum step is used.
#'
#' @return an object of the same class as `x`
#'
#' @export
truncate_steps <- function(x, min_step = NULL, max_step = NULL) {

  if (!inherits(x, c("discrete_pmf", "discrete_weights"))) {
    cli::cli_abort(
      "`x` must be a {.cls discrete_pmf} or {.cls discrete_weights} object."
    )
  }

  if (is.null(min_step)) min_step <- min(x$step)
  if (is.null(max_step)) max_step <- max(x$step)

  if (!is.numeric(min_step) || length(min_step) != 1 || min_step < 0) {
    cli::cli_abort("`min_step` must be a single non-negative numeric value.")
  }
  if (!is.numeric(max_step) || length(max_step) != 1 || max_step < 0) {
    cli::cli_abort("`max_step` must be a single non-negative numeric value.")
  }
  if (min_step >= max_step) {
    cli::cli_abort(c(
      "`min_step` must be less than `max_step`.",
      "i" = "`min_step` is {min_step}.",
      "i" = "`max_step` is {max_step}."
    ))
  }

  out <- x[x$step >= min_step & x$step <= max_step, , drop = FALSE]

  if (nrow(out) == 0) {
    cli::cli_abort(
      "No steps remain after truncation. Check `min_step` and `max_step`."
    )
  }

  if (inherits(x, "discrete_pmf")) {
    out$prob <- out$prob / sum(out$prob)
  }

  class(out) <- class(x)
  out
}
