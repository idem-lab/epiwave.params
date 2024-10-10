#' Add together two epiwave_distribution_massfun objects
#'
#' @param delay1 first epiwave dist object to combine
#' @param delay2 second epiwave dist object to combine
#'
#' @return epiwave_distribution_massfun
#' @export
#'
add_distributions <- function (delay1, delay2) {

  added_dists <- add(delay1, delay2)

  timeseries_added_dists <- create_epiwave_massfun_timeseries(
    dates = infection_days,
    jurisdictions = jurisdictions,
    value = added_dists)
  timeseries_added_dists

}
