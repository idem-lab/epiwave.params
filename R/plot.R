#' Plot a discrete_pmf object
#'
#' @param x a `discrete_pmf` object
#' @param y unused, present for compatibility with the generic `plot` method
#' @param ... additional arguments passed to `ggplot2::geom_col()`
#'
#' @return a `ggplot` object
#'
#' @importFrom ggplot2 ggplot aes geom_col labs theme_minimal
#' @export
plot.discrete_pmf <- function(x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = step, y = prob)) +
    ggplot2::geom_col(width = 0.8, ...) +
    ggplot2::labs(
      x = "Step",
      y = "Probability mass"
    ) +
    ggplot2::theme_minimal()
}

#' Plot a discrete_weights object
#'
#' @param x a `discrete_weights` object
#' @param y unused, present for compatibility with the generic `plot` method
#' @param ... additional arguments passed to `ggplot2::geom_line()`
#'
#' @return a `ggplot` object
#'
#' @importFrom ggplot2 ggplot aes geom_line labs theme_minimal
#' @export
plot.discrete_weights <- function(x, y, ...) {
  ggplot2::ggplot(x, ggplot2::aes(x = step, y = weight)) +
    ggplot2::geom_line(linewidth = 0.8, ...) +
    ggplot2::labs(
      x = "Step",
      y = "Weight"
    ) +
    ggplot2::theme_minimal()
}
