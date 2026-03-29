#' Create distribution object from parametric distribution values
#'
#' @param dist distributional package object
#' @param min_delay optional specification for minimum delay
#' @param max_delay optional specification for maxiumum delay
#'
#' @return epiwave_distribution object
#' @export
parametric_dist_to_curve <- function(dist,
                                     min_delay = NULL,
                                     max_delay = NULL,
                                     min_delay_quantile = 0.00,
                                     max_delay_quantile = 0.99) {
  if (is.null(min_delay)) {
    min_delay <- floor(quantile(dist, min_delay_quantile))
  }
  if (is.null(max_delay)) {
    max_delay <- ceiling(quantile(dist, max_delay_quantile))
  }
  cdf_fun <- function(x) distributional::cdf(dist, x)[[1]]
  out <- discretise_cdf(min_delay, max_delay, cdf_fun, normalise = FALSE)
  out
}
