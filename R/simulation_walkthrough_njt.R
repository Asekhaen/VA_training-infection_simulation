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
#length of time after infection you will test positive on a prevalence survey
test_positive_duration <- 14 

#Disease parameters
prob_ever_symptomatic <- 0.7
min_symptom_delay <- 3
max_symptom_delay <- 10

#Define true infection incidence as an increasing epidemic curve
# that is, the number of new infections per day, per person
true_inf_inc <- seq(from = 0.0005, to = 0.002, length.out = n_days)
# Define the expected number of new infections per day
expected_inf_daily <- true_inf_inc * pop
#plot to visualise
plot(expected_inf_daily~days) 

## TODO: Discuss "vector recycling"

#### Set up structure for data simulations ####
# Simulate number of new infections per day 
#(across the population, therefore new infection = new infected agent)
inf_daily <- rnbinom(n = n_days, mu = expected_inf_daily, size = 100)

# Demonstrating that mu can be a vector - many numbers
rnbinom(n = 1, mu = expected_inf_daily[1], size = 100)
rnbinom(n = 1, mu = expected_inf_daily[2], size = 100)
rnbinom(n = 1, mu = expected_inf_daily[365], size = 100)

# understanding the parameters of distribution
set.seed(2025-02-21)
hist(rnbinom(n = 100, mu = 50, size = 1))
hist(rnbinom(n = 100, mu = 50, size = 50))
hist(rnbinom(n = 100, mu = 50, size = 100))
hist(rnbinom(n = 100, mu = 50, size = 1000))

# plot to visualise
plot(inf_daily ~ days, type = "l", lty = "dotted") 
#add infection prevalence surveys to the plot
abline(v = prev_days, 
       lty = 2)

# create an empty dataframe the length of the disease outbreak 
# to store case timeseries we will be simulating
clinical_cases <- tibble( 
  day = days,
  cases = NA
)

prevalence_surveys <- tibble( #and prevalence surveys
  day = prev_days,
  n_sampled = NA,
  n_positive = NA
)

# and specific information for each agent
agents <- tibble( 
  infection_day = rep(NA, pop),
  ever_symptomatic = rep(NA, pop),
  symptom_delay = rep(NA, pop)
)

#set our seed
set.seed(2025-02-20)

#### Now simulate ####
# Specifically, clinical case timeseries and prevalence surveys with an
# agent-based model, using a for-loop
## try for the first 52 days only, for debugging
# days_sub <- seq_len(prev_days[1] - 1)
# for (day in days_sub) {

for (day in days) { #this will run the below code across all days in our 365 day sequence
  
  # generate new infected agents
  n_new_infections <- inf_daily[day]
  
  new_infected_agents <- tibble(
    infection_day = rep(NA, n_new_infections),
    ever_symptomatic = rep(NA, n_new_infections),
    symptom_delay = rep(NA, n_new_infections)
  )
  
  for (this_infected_agent in seq_len(n_new_infections)) {
    
    # determine for each one whether they will ever be symptomatic
    agent_symptomatic <- rbinom(
      n = 1, 
      size = 1, 
      prob = prob_ever_symptomatic
      )
    
    # convert this to TRUE or FALSE - TRUE if it is a "1"
    # this_agent_ever_symptomatic <- agent_symptomatic == 1
    this_agent_ever_symptomatic <- as.logical(agent_symptomatic)
    
    # assign symptom onset days for the ever_symptomatic ones
    if (this_agent_ever_symptomatic) {
      
      this_agent_symptom_delay <- round(runif(n = 1,
                                              min = min_symptom_delay,
                                              max = max_symptom_delay))
    } else {
      this_agent_symptom_delay <- NA
    }
    # add to the new infected agents block
    new_infected_agents$infection_day[this_infected_agent] <- day
    new_infected_agents$ever_symptomatic[this_infected_agent] <- this_agent_ever_symptomatic
    new_infected_agents$symptom_delay[this_infected_agent] <- this_agent_symptom_delay
  } # close infected agent for loop
  # track these in the full list of agents
  
  # find the next block of empty spaces in the full tibble of agents
  infected_agents_so_far <- sum(!is.na(agents$infection_day))
  infected_agents_to_insert <- seq(infected_agents_so_far + 1, length.out = n_new_infections)
  
  # add the infection information them in this space
  agents[infected_agents_to_insert, ] <- new_infected_agents
  
  # implement clinical case counting for this day
  
  # how many agents became symptomatic today?
  day_of_symptom_onset <- agents$infection_day + agents$symptom_delay
  n_new_cases_today <- sum(day_of_symptom_onset == day, na.rm = TRUE)
  clinical_cases$cases[day] <- n_new_cases_today
  
  # implement prevalence survey on survey days  
  is_prevalence_survey_day <- day %in% prev_days
  if (is_prevalence_survey_day) {
    
    # sample agents at random from the population
    all_agent_ids <- seq_len(pop)
    which_agents_sampled <- sample(all_agent_ids, size = prev_n_sampled)
    agents_sampled <- agents[which_agents_sampled, ]
    
    # tabulate which are positive
    n_positive_this_survey <- 0
    
    # for each tested agent
    for (this_tested_agent in seq_len(prev_n_sampled)) {
      
      # see when they were infected (if at all)
      this_infection_day <- agents_sampled$infection_day[this_tested_agent]
      
      # have they been infected by now?
      this_agent_infected_by_now <- !is.na(this_infection_day)
      
      if (this_agent_infected_by_now) {
        
        # if so, are they positive today?
        this_tested_agent_days_positive <- this_infection_day +
          seq_len(test_positive_duration)
        this_agent_positive_today <- day %in% this_tested_agent_days_positive
        
        # if so, add them to the count        
        if (this_agent_positive_today) {
          n_positive_this_survey <- n_positive_this_survey + 1
        }
      }
    }
    # record the number tested and the number positive
    prevalence_survey_round <- which(prevalence_surveys$day == day)
    prevalence_surveys$n_sampled[prevalence_survey_round] <- prev_n_sampled
    prevalence_surveys$n_positive[prevalence_survey_round] <- n_positive_this_survey
  }
}

head(clinical_cases, 10)

#### Outputs ####
library(readr)
# Case timeseries
clinical_cases
write_csv(x= clinical_cases, file="outputs/clinical_cases.csv")

#Prevalence survey timeseries
prevalence_surveys
#calculate percentage of people positive as a consistent metric
prevalence_surveys$percentage_positive <- (
  prevalence_surveys$n_positive/prevalence_surveys$n_sampled)*100 

write_csv(x = prevalence_surveys, file = "outputs/prevalence_timeseries.csv")

#read in this file
prevalence_surveys_from_file <- read_csv("outputs/prevalence_timeseries.csv")

#Let's come back to our plot of infections
plot(inf_daily ~ days, type = "l", lty = "dotted", col = "grey50")

# add the infections so we can compare
lines(cases ~ days,
      data = clinical_cases)

#plot the positivity from the prevalence surveys
#First we need to calculate the % positive
#prevalence_surveys$percentage_positive <- (prevalence_surveys$n_positive/prevalence_surveys$n_sampled)*100

plot(x = prevalence_surveys$day,
     y = prevalence_surveys$percentage_positive, 
     type = "p")

plot(expected_inf_daily~days) #should approx. match up with trend in prevalence survey plot








