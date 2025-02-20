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

# load packages 
library(tidyverse)

#### Set up parameters and known 'truths' for our simulated disease outbreak ####pop <- 10000
#population and timescale parameters
pop <- 10000
n_days <- 365
days <- seq_len(n_days)

#Survey specific parameters
prev_n_sampled <- 500 #number of people out of the total population sampled in each prevalence survey

n_prevalence_surveys <- 6 #number of times we conduct a prevalence survey
# we then need to figure out when in our timeseries the prevalence surveys are done
prev_days <- round(seq(1, n_days, length.out = n_prevalence_surveys + 2))
prev_days <- head(prev_days, -1) 
# alternative option is prev_days[1:(length(prev_days)-1)]
prev_days <- tail(prev_days, -1)
#alternative option is prev_days[2:(length(prev_days)-1)]
test_positive_duration <- 14 #length of time after infection you will test positive on a prevalence survey

#Disease parameters
prob_ever_symptomatic <- 0.7
min_symptom_delay <- 3
max_symptom_delay <- 10

#Define true infection incidence as an increasing epidemic curve
true_inf_inc <- seq(0.005,0.02, length.out = n_days)
# Define the expected number of new infections per day
expected_inf_daily <- true_inf_inc * pop
plot(expected_inf_daily~days) #plot to visualise












