#' Create distribution object from data
#'
#' @param data_date_1 data column with first date to calculate delay from (earlier date)
#' @param data_date_2 data column with second date to calculate delay from (later date)
#' @param min_delay_days optional specification for minimum delay
#' @param max_delay_days optional specification for maxiumum delay
#' @param min_delay_quantile optional specification for quantile to select
#'  minimum delay IF min_delay_days hasn't been set. Defaults to 0.
#' @param max_delay_quantile optional specification for quantile to select
#'  maxiumum delay IF max_delay_days hasn't been set. Defaults to 0.99.
#'
#' @return epiwave_distribution object
#' @export
data_to_distribution <- function (data_date_1,
                                  data_date_2,
                                  min_delay_days = NULL,
                                  max_delay_days = NULL,
                                  min_delay_quantile = 0.00,
                                  max_delay_quantile = 0.99) {

  day_diff <- data_date_2 - data_date_1
  cdf_fun <- stats::ecdf(day_diff)

  if (is.null(min_delay)) {
    min_delay <- floor(quantile(cdf_fun, min_delay_quantile))
  }
  if (is.null(max_delay)) {
    max_delay <- ceiling(quantile(cdf_fun, max_delay_quantile))
  }

  out <- create_epiwave_massfun(
    min_delay_days, max_delay_days,
    cdf_fun, normalise = TRUE)
  out

}
