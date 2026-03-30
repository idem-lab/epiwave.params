#' Calculate a convolution matrix
#'
#' @description Constructs a forward convolution matrix from a `discrete_pmf`
#'   or `discrete_pmf_series` object. The resulting matrix can be used to
#'   convolve a time series of infections forward through a delay distribution.
#'
#'   When `delay_distribution` is a single `discrete_pmf`, the same PMF is
#'   applied at every timepoint. When it is a `discrete_pmf_series`, each
#'   timepoint uses its corresponding PMF, enabling time-varying delay
#'   distributions.
#'
#'   Note that only `discrete_pmf` objects are accepted, not `discrete_curve`
#'   objects, since convolution requires a normalised probability mass function.
#'   To verify that a `discrete_pmf` sums to 1, use `sum(as.data.frame(x)$mass)`.
#'
#' @srrstats {PD3.5} Discrete summation is used and normalisation is required
#'   for this operation to be valid.
#'
#' @param delay_distribution a `discrete_pmf` or `discrete_pmf_series` object.
#'   If a `discrete_pmf_series`, its length must equal `n`.
#' @param n a positive integer giving the number of timepoints, which
#'   determines the dimensions of the output matrix.
#'
#' @return an `n` by `n` numeric matrix for forward convolution
#'
#' @export
get_convolution_matrix <- function(delay_distribution, n) {

  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != as.integer(n)) {
    cli::cli_abort("`n` must be a single positive integer.")
  }

  if (inherits(delay_distribution, "discrete_pmf")) {
    # constant case — same PMF for all timepoints
    evaluate_fun <- function(col_idx) {
      evaluate_pmf(delay_distribution, day_diff[, col_idx])
    }

  } else if (inherits(delay_distribution, "discrete_pmf_series")) {
    if (length(delay_distribution$values) != n) {
      cli::cli_abort(c(
        "`delay_distribution` and `n` must be compatible.",
        "i" = "`delay_distribution` has {length(delay_distribution$values)} timepoints.",
        "i" = "`n` is {n}."
      ))
    }
    # time-varying case — different PMF per timepoint
    evaluate_fun <- function(col_idx) {
      evaluate_pmf(delay_distribution$values[[col_idx]], day_diff[, col_idx])
    }

  } else {
    cli::cli_abort(
      "`delay_distribution` must be a {.cls discrete_pmf} or {.cls discrete_pmf_series} object."
    )
  }

  day_diff <- matrix(NA, n, n)
  day_diff <- row(day_diff) - col(day_diff)

  con_mat <- do.call(cbind, lapply(seq_len(n), evaluate_fun))
  con_mat
}

#' @noRd
evaluate_pmf <- function(lookup, day_diff_col) {
  lookup <- as.data.frame(lookup)
  day_diff_col[] <- lookup$mass[
    match(day_diff_col, lookup$delays)
  ]
  day_diff_col[is.na(day_diff_col)] <- 0
  day_diff_col
}
