# setup ####

library(tidyverse)
library(fit)
library(fitdistrplus)


# read data ####
raw_data <- readxl::read_xlsx("./overbooking/Flight-Overbooking-Data.xlsx")

# simple clean up
flight_dt <- raw_data%>% 
  select(1:5) %>% 
  janitor::clean_names()

# EDA ####

# overview
flight_dt %>% 
  skimr::skim()

# modeling the demand ####
flight_dt %>% 
  ggplot(aes(x=demand)) +
  geom_histogram() +
  theme_light()

# checking the empirical distribution
plotdist(flight_dt$demand, discrete = T)

# what are the distribution candidates?
descdist(flight_dt$demand, boot=1000, discrete = T)

# all then is possible, lets fit a poisson and a normal
fitdist(flight_dt$demand, "pois", discrete = T) %T>%
  plot() %>% 
  summary()

fitdist(flight_dt$demand, "norm", discrete = T) %>%
  plot() %>% 
  summary()

# Emp CDF of the Poisson is a little better ans IAC also is marginally better
demand.pois <- fitdist(flight_dt$demand, "pois", discrete = T)

# Rate EDA
flight_dt %>% 
  ggplot(aes(x=rate, y=demand)) +
  geom_point() +
  theme_light()

plotdist(flight_dt$rate, discrete = T)

# Modeling ####

n <- 10000 # simulations
overbook <- 15
capacity <- 150
showup_rate <- mean(flight_dt$rate)

# demand simulation
simulateDemand <- function(overbook, n, capacity, showup_rate) {
  tibble(demand = rpois(n, demand.pois$estimate)) %>% 
    # booked: demand inside capacity (flight seets) 
    mutate( booked = map_dbl(demand, ~min( .x, overbook+capacity ) )) %>% 
    # show-ups and no shows
    mutate( shows    = map_dbl(booked, ~rbinom(1,.x,showup_rate)),
            no_shows = booked - shows ) %>%
    # shop-up rate
    mutate( showup_rate = shows/booked ) %>%
    # calc overbook and empty seats
    mutate( overbooked  = shows - capacity,
            empty_seats = capacity - shows ) %>%
    # remove negative values
    mutate( overbooked  = map_dbl(overbooked, ~max(.x,0)),
            empty_seats = map_dbl(empty_seats, ~max(.x, 0))) %>% 
    return()  
}

# simulating one case ####
sim <- simulateDemand(15,10000,150,mean(flight_dt$rate))

# what we got
sim

# lets visualize the overbooked passengers distribution
sim %>% 
  count(overbooked)

plotdist(sim$overbooked)

# chance to have 2 or less bumped pass
bumped_more_2 <- sim %>% 
  count(overbooked) %>% 
  filter(overbooked>2) %>% 
  summarise( total = sum(n) ) %>% 
  unlist()

1-(bumped_more_2/10000)

# simulation ####

# lets find the optimal overbook to get max of 2 bumped passengers in 95% of situations

# before that, lets create a auxiliary function
probBumpedPass <- function(simulation, nPass){
  # calc the probability of the number of bumped passengers be less then nPass in a simulation
  simulation %>% 
    count(overbooked) %>% 
    filter(overbooked<=nPass) %>% 
    summarise( total = sum(n)/10000 ) %>% 
    unlist() %>% 
    return()
}

# looking the behavior of the probability to get 2 (or 5) less passengers bumped
tibble(overbook=1:20) %>% 
  mutate( simulation = map(overbook, simulateDemand, n=10000, capacity=150, showup_rate=mean(flight_dt$rate))) %>% 
  mutate( prob2BumpPass = map_dbl(simulation, probBumpedPass, nPass=2),
          prob5BumpPass = map_dbl(simulation, probBumpedPass, nPass=5)) %>% 
  pivot_longer(cols=c(-overbook, -simulation), names_to = "bumped", values_to = "prob") %>% 
  ggplot(aes(x=overbook, y=prob, color=bumped)) +
  geom_hline(yintercept=0.95, linetype="dashed") + 
  geom_line() +
  geom_point() +
  theme_light()
  
# we can see that offering 13 additional seats (over plain capacity) we have less than 5% of chance to bumped more than 2 passengers
# Offering 18 additional seats (over plain capacity) we have less than 5% of chance to bump more than 5 passengers

# dependence ####

# we assume that the showup rate is fixed, is it?
cor.test(flight_dt$demand, flight_dt$rate)

# no it isn't, so we have to modeling this dependence

# lets make a simple linear model
rate_model <- lm(rate ~ demand, data = flight_dt)

# what we got?
summary(rate_model)
par(mfrow=c(2,2))
plot(rate_model)
par(mfrow=c(1,1))

# another simulation model considering the dependency between showup rate and demand
simulateDemandShowUpModel <- function(overbook, n, capacity, showup_model) {
  # generate a demand simulation
  demSim <- tibble(demand = rpois(n, demand.pois$estimate))
  # based in showup model calc a predicted showup_rate for each demand
  demSim$predShowup_rate = predict(showup_model, newdata=demSim)
  
  # complete the simulation
  demSim %>% 
    # booked: demand inside capacity (flight seats) 
    mutate( booked = map_dbl(demand, ~min( .x, overbook+capacity ) )) %>% 
    # compute the show-ups and no shows
    mutate( shows    = map2_dbl(booked, predShowup_rate, ~rbinom(1,.x,.y)),
            no_shows = booked - shows ) %>%
    # shop-up rate
    mutate( showup_rate = shows/booked ) %>%
    # calc overbook and empty seats
    mutate( overbooked  = shows - capacity,
            empty_seats = capacity - shows ) %>%
    # remove negative values
    mutate( overbooked  = map_dbl(overbooked, ~max(.x,0)),
            empty_seats = map_dbl(empty_seats, ~max(.x, 0))) %>% 
    return()  
}

# simulating one case ####
sim <- simulateDemandShowUpModel(15,10000,150,rate_model)

# what we got
sim

# lets visualize the overbooked passengers distribution
sim %>% 
  count(overbooked)

plotdist(sim$overbooked)

# chance to have 2 or less bumped pass
bumped_more_2 <- sim %>% 
  count(overbooked) %>% 
  filter(overbooked>2) %>% 
  summarise( total = sum(n) ) %>% 
  unlist()

1-(bumped_more_2/10000)

# we got a significatily different distribuition
# the bumped passengers are more flat


# looking the behavior of the probability to get 2 (or 5) less passengers bumped
# in the new model
tibble(overbook=1:20) %>% 
  mutate( simulation = map(overbook, simulateDemandShowUpModel, n=10000, capacity=150, showup_model= rate_model)) %>% 
  mutate( prob2BumpPass = map_dbl(simulation, probBumpedPass, nPass=2),
          prob5BumpPass = map_dbl(simulation, probBumpedPass, nPass=5)) %>% 
  pivot_longer(cols=c(-overbook, -simulation), names_to = "bumped", values_to = "prob") %>% 
  ggplot(aes(x=overbook, y=prob, color=bumped)) +
  geom_hline(yintercept=0.95, linetype="dashed") + 
  geom_line() +
  geom_point() +
  theme_light()

# now we got different results
# probability of 95% to get 2 or less bumped pass: 8 over capacity seats
# probability of 95% to get 5 or less bumped pass: 12 over capacity seats

# economic view ####
ticket_prc <- 314
transf_fee <- 60
bumped_cost <- 400

demand_sim <- simulateDemandShowUpModel(15, n=10000, capacity=150, showup_model= rate_model)

finance_sim <- demand_sim %>% 
  mutate(
    revenue       = booked * ticket_prc,
    noshow_cost   = no_shows * transf_fee,
    overbook_cost = overbooked * bumped_cost,
    profit        = revenue - (noshow_cost+overbook_cost)
  )

plotdist(finance_sim$profit)

calcFinanceResults <- function(simulation, ticket_prc=314, transf_fee=60, bumped_cost=400){
  simulation %>% 
    mutate(
      revenue       = booked * ticket_prc,
      noshow_cost   = no_shows * transf_fee,
      overbook_cost = overbooked * bumped_cost,
      profit        = revenue - (noshow_cost+overbook_cost)
    ) %>% 
    return()
}

tibble(overbook=1:20) %>% 
  mutate( simulation = map(overbook, simulateDemandShowUpModel, n=10000, capacity=150, showup_model= rate_model)) %>% 
  mutate( simulation = map(simulation, calcFinanceResults))
  