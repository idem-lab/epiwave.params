#' Convert a discrete_pmf_series to a tsibble
#'
#' @description Converts a `discrete_pmf_series` object to a
#'   [`tsibble::tsibble`] with the series index as the time index and the
#'   `discrete_pmf` objects stored in a list column named `pmf`. This
#'   functionality allows users to opt in to tidy time‑series workflows.
#'
#' @param x a `discrete_pmf_series` object
#' @param ... additional arguments passed to [`tsibble::as_tsibble`]
#'
#' @return a `tsibble` with columns `index` and `pmf`
#'
#' @importFrom tsibble as_tsibble
#' @export
#' @method as_tsibble discrete_pmf_series
as_tsibble.discrete_pmf_series <- function(x, ...) {
  as_tsibble_discrete_series(
    x,
    value_col = "pmf",
    ...
  )
}

#' Convert a discrete_weights_series to a tsibble
#'
#' @description Converts a `discrete_weights_series` object to a
#'   [`tsibble::tsibble`] with the series index as the time index and the
#'   `discrete_weights` objects stored in a list column named `weights`.
#'
#' @param x a `discrete_weights_series` object
#' @param ... additional arguments passed to [`tsibble::as_tsibble`]
#'
#' @return a `tsibble` with columns `index` and `weights`
#'
#' @importFrom tsibble as_tsibble
#' @export
#' @method as_tsibble discrete_weights_series
as_tsibble.discrete_weights_series <- function(x, ...) {
  as_tsibble_discrete_series(
    x,
    value_col = "weights",
    ...
  )
}

#' @noRd
as_tsibble_discrete_series <- function(x, value_col, ...) {
  if (!requireNamespace("tsibble", quietly = TRUE)) {
    cli::cli_abort(
      "Package {.pkg tsibble} is required. Install with {.code install.packages('tsibble')}."
    )
  }
  df <- tibble::tibble(
    index = x$index
  )

  df[[value_col]] <- x$values

  tsibble::as_tsibble(
    df,
    index = index,
    ...
  )
}
