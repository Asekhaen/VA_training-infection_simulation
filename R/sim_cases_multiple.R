# Demonstration of model-based inference of infection incidence

# load packages 
library(tidyverse)

# We would like to know what the infection incidence is and how it is changing
# over time.

# We have two observed datasets telling us about: the number of clinical
# cases over time, and on the infection prevalence on certain days:
clinical_cases_observed <- read_csv("outputs/clinical_cases.csv")
prevalence_observed <- read_csv("outputs/prevalence_timeseries.csv")

# We also have a *generative model* for how infection incidence (changing
# linearly over time) can create datasets of this type. This model incorporates
# our knowledge about how the observations are made.

# this is the simulation code, wrapped into a function. There are two unknown
# parameters: the infection incidence at the start, and the infection incidence
# at the end. The infection incidence is assumed to change linearly between
# these. For now, we will assume we know the other key parameters.
sim_cases <- function(
    inf_inc_start,
    inf_inc_end,
    test_positive_duration = 14, 
    prob_ever_symptomatic = 0.7,
    min_symptom_delay = 3,
    max_symptom_delay = 10
) {
  
  n_prevalence_surveys <- 6
  prev_n_sampled <- 500
  pop <- 10000
  n_days <- 365
  days <- seq_len(n_days)
  
  # we then need to figure out when in our timeseries the prevalence surveys are done
  prev_days <- round(seq(1, n_days, length.out = n_prevalence_surveys + 2))
  prev_days <- head(prev_days, -1) 
  # alternative option is prev_days[1:(length(prev_days)-1)]
  prev_days <- tail(prev_days, -1)
  #alternative option is prev_days[2:(length(prev_days)-1)]
  
  #Define true infection incidence as an increasing epidemic curve
  # that is, the number of new infections per day, per person
  true_inf_inc <- seq(from = inf_inc_start,
                      to = inf_inc_end,
                      length.out = n_days)
  # Define the expected number of new infections per day
  expected_inf_daily <- true_inf_inc * pop
  
  #### Set up structure for data simulations ####
  # Simulate number of new infections per day 
  #(across the population, therefore new infection = new infected agent)
  inf_daily <- rnbinom(n = n_days, mu = expected_inf_daily, size = 100)
  
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
  
  #### Now simulate ####
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
  
  # return these
  list(clinical_cases = clinical_cases,
       prevalence_surveys = prevalence_surveys)
  
}

# We can *propose* values of the incidence parametersm and simulate alternative
# datasets.

proposed_parameters <- list(inf_inc_start = 0.0003,
                            inf_inc_end = 0.0021)

# plot the implied infection incidence
days <- clinical_cases_observed$day
inf_inc_proposed <- seq(from = proposed_parameters$inf_inc_start,
                        to = proposed_parameters$inf_inc_end,
                        length.out = length(days))

par(mfrow = c(1, 2))
plot(inf_inc_proposed ~ days,
     type = "l",
     ylim = c(0, 0.01))
title(main = "proposed\ninfection\nincidence")


# now simulate a fake clinical cases dataset from this parameter set
sim <- sim_cases(inf_inc_start = proposed_parameters$inf_inc_start,
                 inf_inc_end = proposed_parameters$inf_inc_end)

# plot the observed data and overplot the simulation
ylim <- range(c(clinical_cases_observed$cases,
                sim$clinical_cases$cases))

plot(cases ~ day,
     data = clinical_cases_observed,
     type = "p",
     col = "grey50",
     cex = 0.5,
     ylim = ylim)

lines(cases ~ day,
     data = sim$clinical_cases)

title(main = "black = simulation\ngrey = truth")






