# source('R/pmf_dev.R')
library(dplyr)
library(epiwave.params)

dist <- distributional::dist_weibull(shape = 2, scale = 3)
delay_probs_from_dist <- parametric_dist_to_distribution(dist)
# manual test
manual_pmf <- (cdf(dist,0:7)[[1]] - cdf(dist,-1:6)[[1]])
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

sero_curve_data <- read.csv('data_positivity_draft.csv')


out <- data_to_curve(sero_curve_data$days_since_onset,
                     sero_curve_data$p_pos_median)



