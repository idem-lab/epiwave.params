#' Subset a discrete_pmf_series
#'
#' @description Subsets a `discrete_pmf_series` object by position, logical
#'   vector, or index value, returning a new `discrete_pmf_series` with the
#'   selected elements. Class and index alignment are preserved.
#'
#' @param x a `discrete_pmf_series` object
#' @param i integer positions, a logical vector of the same length as the
#'   series, or a vector of index values matching the type of `x$index`
#' @param ... unused, present for compatibility with the generic
#'
#' @return a `discrete_pmf_series` object
#'
#' @export
"[.discrete_pmf_series" <- function(x, i, ...) {
  subset_discrete_series(x, i)
}

#' Subset a discrete_weights_series
#'
#' @description Subsets a `discrete_weights_series` object by position, logical
#'   vector, or index value, returning a new `discrete_weights_series` with the
#'   selected elements. Class and index alignment are preserved.
#'
#' @param x a `discrete_weights_series` object
#' @param i integer positions, a logical vector of the same length as the
#'   series, or a vector of index values matching the type of `x$index`
#' @param ... unused, present for compatibility with the generic
#'
#' @return a `discrete_weights_series` object
#'
#' @export
"[.discrete_weights_series" <- function(x, i, ...) {
  subset_discrete_series(x, i)
}

#' @noRd
subset_discrete_series <- function(x, i) {

  # validate i type first
  if (!is.numeric(i) && !is.logical(i) &&
      !inherits(i, c("Date", "POSIXct", "POSIXt"))) {
    cli::cli_abort(
      "`i` must be an integer, logical, or index vector matching the type of
      the series index."
    )
  }

  # logical vector — check length then pass through to positional
  if (is.logical(i)) {
    if (length(i) != length(x$index)) {
      cli::cli_abort(c(
        "Logical index must be the same length as the series.",
        "i" = "Series has {length(x$index)} timepoints.",
        "i" = "`i` has length {length(i)}."
      ))
    }

    # Date or POSIXct index — match against series index
  } else if (inherits(i, c("Date", "POSIXct", "POSIXt"))) {
    if (!inherits(x$index, c("Date", "POSIXct", "POSIXt"))) {
      cli::cli_abort(
        "Cannot subset by date when the series index is not a date vector."
      )
    }
    i <- match(i, x$index)
    if (anyNA(i)) {
      cli::cli_abort("Some index values were not found in the series.")
    }

    # numeric/integer — always treated as positional
    # note: if the series index is numeric, use match() explicitly via date
    # subsetting above; integer positions are unambiguous
  } else if (is.numeric(i)) {
    if (any(i < 1) || any(i > length(x$index))) {
      cli::cli_abort(c(
        "Positional index out of bounds.",
        "i" = "Series has {length(x$index)} timepoints.",
        "i" = "Index values must be between 1 and {length(x$index)}."
      ))
    }
    i <- as.integer(i)
  }

  structure(
    list(
      index = x$index[i],
      values = x$values[i]
    ),
    class = class(x)
  )
}
