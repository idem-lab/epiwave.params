#' Construct a forward convolution matrix from a discrete PMF
#'
#' @description Constructs a forward convolution matrix from a `discrete_pmf`
#'   or `discrete_pmf_series` object. The resulting matrix can be multiplied
#'   by a time series vector to convolve it forward through a delay
#'   distribution.
#'
#'   When `pmf` is a single `discrete_pmf`, the same PMF is applied at every
#'   timepoint. When it is a `discrete_pmf_series`, each timepoint uses its
#'   corresponding PMF, enabling time-varying delay distributions.
#'
#'   Only `discrete_pmf` objects are accepted, not `discrete_curve` objects,
#'   since this operation requires a normalised probability mass function.
#'
#' @srrstats {PD3.5} Discrete summation is used following Cori et al. (2013,
#'   AJE) and Gostic et al. (2020, PLOS Comp Bio), who demonstrate its
#'   suitability for discrete-time epidemiological models. Normalisation of
#'   the input PMF guarantees a finite sum (PD3.5a).
#'
#' @param pmf a `discrete_pmf` or `discrete_pmf_series` object. A single
#'   `discrete_pmf` is applied uniformly across all timepoints. A
#'   `discrete_pmf_series` enables time-varying delays, where each element
#'   corresponds to one timepoint and the series length must equal `n`.
#' @param n a positive integer giving the number of timepoints, which
#'   determines the dimensions of the output matrix.
#'
#' @return an `n` by `n` numeric matrix. To apply the convolution, multiply
#'   the matrix by a time series vector of length `n`, e.g.
#'   `new_convolution_matrix(pmf, n) %*% time_series`.
#'
#' @examples
#' pmf <- as_discrete_pmf(distributional::dist_gamma(shape = 3, rate = 0.5))
#' new_convolution_matrix(pmf, n = 10)
#'
#' @export
new_convolution_matrix <- function(pmf, n) {

  if (!is.numeric(n) || length(n) != 1 || n < 1 || n != as.integer(n)) {
    cli::cli_abort("`n` must be a single positive integer.")
  }

  if (!inherits(pmf, c("discrete_pmf", "discrete_pmf_series"))) {
    cli::cli_abort(
      "`pmf` must be a {.cls discrete_pmf} or {.cls discrete_pmf_series} object."
    )
  }

  day_diff <- matrix(NA, n, n)
  day_diff <- row(day_diff) - col(day_diff)

  if (inherits(pmf, "discrete_pmf")) {
    evaluate_fun <- function(col_idx) {
      evaluate_pmf(pmf, day_diff[, col_idx])
    }
  } else {
    if (length(pmf$values) != n) {
      cli::cli_abort(c(
        "`pmf` and `n` must be compatible.",
        "i" = "`pmf` has {length(pmf$values)} timepoints.",
        "i" = "`n` is {n}."
      ))
    }
    evaluate_fun <- function(col_idx) {
      evaluate_pmf(pmf$values[[col_idx]], day_diff[, col_idx])
    }
  }

  do.call(cbind, lapply(seq_len(n), evaluate_fun))
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
