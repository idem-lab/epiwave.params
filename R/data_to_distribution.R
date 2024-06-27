#' Create distribution object from data
#'
#' @param data_date_1 data column with first date to calculate delay from (earlier date)
#' @param data_date_2 data column with second date to calculate delay from (later date)
#' @param min_delay optional specification for minimum delay
#' @param max_delay optional specification for maxiumum delay
#'
#' @return epiwave_distribution object
#' @export
data_to_distribution <- function (data_date_1,
                                  data_date_2,
                                  min_delay = NULL,
                                  max_delay = NULL) {

  day_diff <- data_date_2 - data_date_1
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
