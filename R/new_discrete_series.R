#' Create a time-indexed series of discrete_pmf or discrete_weights objects
#'
#' @description Constructs a `discrete_pmf_series` or `discrete_weights_series`
#'   object from a time index and either a single `discrete_pmf` or
#'   `discrete_weights` (replicated across all timepoints), or a list of such
#'   objects of the same length as the index. The class of the returned object
#'   is determined by the class of the supplied `values`.
#'
#' @param values either a single `discrete_pmf` or `discrete_weights` object, or
#'   a list of such objects with the same length as `index`. All elements must
#'   be the same class.
#' @param index a vector of dates, integers, or other ordered values providing
#'   a temporal index. Must be strictly increasing and have no missing values.
#'
#' @return a `discrete_pmf_series` or `discrete_weights_series` object: a list
#'   with components `index` and `values`, where `values` is a list of
#'   `discrete_pmf` or `discrete_weights` objects aligned to the index.
#'
#' @export
new_discrete_series <- function(values, index) {
  validate_series_index(index)
  n <- length(index)

  if (inherits(values, c("discrete_pmf", "discrete_weights"))) {
    out_class <- paste0(class(values)[1], "_series")
    cli::cli_inform(c(
      "i" = "A single {.cls {class(values)[1]}} was supplied.",
      "*" = "It will be replicated across all {n} timepoints."
    ))
    values <- rep(list(values), n)
  } else if (is.list(values)) {
    classes <- vapply(values, function(x) class(x)[1], character(1))
    if (length(unique(classes)) > 1) {
      cli::cli_abort(c(
        "All elements of `values` must be the same class.",
        "i" = "Found: {.cls {unique(classes)}}."
      ))
    }
    if (!classes[1] %in% c("discrete_pmf", "discrete_weights")) {
      cli::cli_abort(
        "All elements of `values` must be {.cls discrete_pmf} or
        {.cls discrete_weights} objects."
      )
    }
    out_class <- paste0(classes[1], "_series")
    validate_series_list(values, n, classes[1])
  } else {
    cli::cli_abort(
      "`values` must be a {.cls discrete_pmf}, {.cls discrete_weights},
      or a list of either."
    )
  }

  structure(
    list(index = index, values = values),
    class = out_class
  )
}
