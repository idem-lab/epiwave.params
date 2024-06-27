#' Create distribution object from data
#'
#' @param data delay data
#' @param min_delay optional specification for minimum delay
#' @param max_delay optional specification for maxiumum delay
#'
#' @return epiwave_distribution object
#' @export
data_to_distribution <- function (data,
                                  min_delay = NULL,
                                  max_delay = NULL) {

  day_diff <- data$notif_date - data$sym_date
  cdf_fun <- stats::ecdf(day_diff)

  if (is.null(min_delay)) {
    min_delay <- floor(quantile(cdf_fun, 0.00))
  }
  if (is.null(max_delay)) {
    max_delay <- ceiling(quantile(cdf_fun, 0.99))
  }

  out <- create_epiwave_massfun(
    min_delay, max_delay,
    cdf_fun, normalise = TRUE)
  out

}
