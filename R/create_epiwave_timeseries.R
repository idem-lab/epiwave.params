#' Expand constant value into long tibble
#'
#' @description The epiwave model functions expect data in a long format,
#'  which is structed to have a value for every unique date and jurisdiction
#'  pair. This function create a tibble of this structure out of a single
#'  value that should be replicated in each cell.
#'
#' @param dates infection dates sequence
#' @param jurisdictions jurisdiction names
#' @param value value to be replicated in each cell
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @return long tibble with constant value replicated
#' @export
#Assign superclass structure for timeseries data (i.e. distributions)
create_epiwave_timeseries <- function (dates,
                                           jurisdictions,
                                           value) {

  long_unique <- expand.grid(date = dates,
                             jurisdiction = jurisdictions)
  long_combined <- long_unique |>
    dplyr::mutate(value = value) |> #consider another name for constant_val
    #dplyr::mutate(!!col_name := constant_val) |>
    tibble::tibble()

  class(long_combined) <- c("epiwave_timeseries", class(long_combined))

  long_combined

  #also need another function that creates a proper time varying time series
  #(i.e. not outputting constant value)
  #:= name injection, assigns variable name to column name
}

#' Expand distribution into long tibble
#'
#' @description The epiwave model functions expect data in a long format,
#'  which is structured to have a value for every unique date and jurisdiction
#'  pair. This function create a tibble of this structure out of a single
#'  distribution that should be replicated in each cell.
#'
#' @param dates infection dates sequence
#' @param jurisdictions jurisdiction names
#' @param value distribution to be replicated in each cell
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @return long tibble with distribution replicated
#' @export
create_epiwave_massfun_timeseries <- function (dates,
                                                jurisdictions,
                                                value) {

  timeseries <- create_epiwave_timeseries(
    dates = infection_days,
    jurisdictions = jurisdictions,
    value = list(value))

  class(timeseries) <- c("epiwave_massfun_timeseries",
                         class(value),  # CHECK THIS
                         class(timeseries))
  timeseries

}

#' Title
#'
#' @param dates infection dates sequence
#' @param jurisdictions jurisdiction names
#' @param value constant value to be replicated in each cell
#'
#' @return long tibble with fixed value replicated
#' @export
create_epiwave_fixed_timeseries <- function (dates,
                                                 jurisdictions,
                                                 value) {

  timeseries <- create_epiwave_timeseries(
    dates = infection_days,
    jurisdictions = jurisdictions,
    value = value)
  #
  # distribution_timeseries <- dplyr::rename(
  #   notif_full_delay_dist,
  #   distribution = value)

  class(timeseries) <- c("epiwave_fixed_timeseries",
                                      class(timeseries))
  timeseries

}

#' Expand distribution into long tibble
#'
#' @description The epiwave model functions expect data in a long format,
#'  which is structured to have a value for every unique date and jurisdiction
#'  pair. This function create a tibble of this structure out of a single
#'  distribution that should be replicated in each cell.
#'
#' @param dates infection dates sequence
#' @param jurisdictions jurisdiction names
#' @param car x
#' @param chr_prior x
#'
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#'
#' @return list
#' @export
create_epiwave_greta_timeseries <- function (dates,
                                             jurisdictions,
                                             car,
                                             chr_prior) { # greta::uniform(0, 1)

  long_unique <- expand.grid(date = dates,
                             jurisdiction = jurisdictions) |>
    tibble::tibble()

  # chr_prior <- greta::uniform(0, 1)
  dim(chr_prior) <- nrow(long_unique)

  if("epiwave_timeseries" %in% class(car)) {
    car <- car$value
  }

  ihr_greta <- car * chr_prior

  long_combined <- list(timeseries = long_unique,
                        ihr = ihr_greta)

  class(long_combined) <- c("epiwave_greta_timeseries",
                            "epiwave_timeseries",
                            class(long_combined))

  long_combined
}

