#' Create distribution object from parametric distribution values
#'
#' @param dist distributional package object
#' @param min_delay_days optional specification for minimum delay
#' @param max_delay_days optional specification for maxiumum delay
#' @param min_delay_quantile optional specification for quantile to select
#'  minimum delay IF min_delay_days hasn't been set. Defaults to 0.
#' @param max_delay_quantile optional specification for quantile to select
#'  maxiumum delay IF max_delay_days hasn't been set. Defaults to 0.99.
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
    min_delay_days, max_delay_days,
    cdf_fun, normalise = TRUE)
  out

}
