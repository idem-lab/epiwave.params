#' Create distribution object from parametric distribution values
#'
#' @param dist distributional package object
#' @param min_delay optional specification for minimum delay
#' @param max_delay optional specification for maxiumum delay
#'
#' @importFrom distributional cdf
#'
#' @return epiwave_distribution object
#' @export
parametric_dist_to_distribution <- function (dist,
                                             min_delay_days = NULL,
                                             max_delay_days = NULL,
                                             min_delay_quantile = 0.00,
                                             max_delay_quantile = 0.99) {

  # needs error that we either have a min_delay_days OR min_delay_quantile/
  # and max_delay_days OR max_delay_quantile

  # this quantile is the S3 method for quantile from distributional pkg
  if (is.null(min_delay_days)) {
    min_delay <- floor(quantile(dist, min_delay_quantile))
  }
  if (is.null(max_delay_days)) {
    max_delay <- ceiling(quantile(dist, max_delay_quantile))
  }

  # when distributional::cdf is applied to a sequence (of x) it returns a list
  cdf_fun <- function(x) distributional::cdf(dist, x)[[1]]

  out <- create_epiwave_massfun(
    min_delay, max_delay,
    cdf_fun, normalise = TRUE)
  out

}
