# source('R/pmf_dev.R')
library(dplyr)
library(epiwave.params)
library(distributional)

dist <- distributional::dist_weibull(shape = 2, scale = 3)
delay_probs_from_dist <- parametric_dist_to_distribution(dist)

# manual test
manual_pmf <- (distributional::cdf(dist,0:7)[[1]] -
                 distributional::cdf(dist,-1:6)[[1]])
manual_pmf <- manual_pmf/sum(manual_pmf)
all.equal(manual_pmf,delay_probs_from_dist$mass)

data <- data.frame(sym_date = rep(seq(from = as.Date('2023-01-2'),
                                      to = as.Date('2023-03-4'),
                                      by = 'day'),
                                  each = 20))
data$notif_date <- data$sym_date + rpois(nrow(data), 4)
delay_probs_from_data <- data_to_distribution(data_date_1 = data$sym_date,
                                              data_date_2 = data$notif_date)

combined <- add(delay_probs_from_dist,
                delay_probs_from_data)


# adding more than one test <- add(combined, delay_probs_from_data)

sero_curve_data <- read.csv('tests/test_workflows/data_positivity_draft.csv')


out <- data_to_curve(sero_curve_data$days_since_onset,
                     sero_curve_data$p_pos_median)


    ihr <- create_epiwave_greta_timeseries(
      dates,
      jurisdictions,
      prior = greta::uniform(0, 1)
    )


    inc <- parametric_dist_to_distribution(
      distributional::dist_weibull(
        shape = 2,
        scale = 3
      )
    )

    case_delay <- add(
      incubation_period,
      onset_to_notification_delay
    )


    observation_models <- define_observation_model(

      dates,
      jurisdictions,

      cases = define_observation_data(
        timeseries_data = case_timeseries,
        delay_from_infection = case_delay,
        proportion_infections = car
      )



      hospitalisations = define_observation_data(
        timeseries_data = hosp_timeseries,
        delay_from_infection = hosp_delay,
        proportion_infections = ihr
      )


      # ,
      # other
    )


    define_infection_model(
      observation_data,
      infection_model_type = gp(
        effect_type = 'growth_rate'
      )
    )


