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
                                             min_delay = NULL,
                                             max_delay = NULL) {

  # this quantile is the S3 method for quantile from distributional pkg
  if (is.null(min_delay)) {
    min_delay <- floor(quantile(dist, 0.00))
  }
  if (is.null(max_delay)) {
    max_delay <- ceiling(quantile(dist, 0.99))
  }

  # when distributional::cdf is applied to a sequence (of x) it returns a list
  cdf_fun <- function(x) distributional::cdf(dist, x)[[1]]

  out <- create_epiwave_massfun(
    min_delay, max_delay,
    cdf_fun, normalise = TRUE)
  out

}
