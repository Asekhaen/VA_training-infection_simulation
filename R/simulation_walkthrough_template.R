#### Project aim: ####
#   simulate two different data timeseries for a disease outbreak
#   Timeseries 1 - case incidence timeseries (daily resolution)
#   Timeseries 2 - infection prevalence timeseries (sparse resolution)

#### Project method overview - how we are going to meet the aim ####
# 1) Generate agents at the point of infection (set up an agent-based model)
# 2) For each agent, determine whether they have symptoms and time to symptom onset
# 3) Loop through time day-by-day, simulating the disease outbreak and specifically: 
#       a) The number of reported cases on each day (case incidence timeseries)
#       b) Infection prevalence surveys on some days (infection prevalence timeseries)

#Minimum resolution is daily

# libraries ----
library(tidyverse)

# set up population and time-scale parameters
pop <- 10000
n_days <- 365
days <- seq_len(n_days)

# set up our disease paraemeters
