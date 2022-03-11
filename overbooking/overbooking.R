library(tidyverse)  

# Parameters

avg_demand <- 150 # passengers
pass_show_up <- .92
capacity <- 134 # passengers
ticket_prc <- 314
transf_fee <- 60
bumped_cost <- 400

# Model
overbook_lim <- 0
actual_demand <- 154  # simulations
booking_capacity <- capacity+overbook_lim
booked <- min(actual_demand, booking_capacity)
read_to_board <- 123 # simulation
no_shows <- booked-read_to_board
boarding <- min(capacity, read_to_board)
bumped <- read_to_board-boarding

revenue_tickets <- ticket_prc * booked
revenue_no_shows <- transf_fee * no_shows
overbook_cost <- bumped_cost*bumped
net_revenue <- revenue_tickets+revenue_no_shows-overbook_cost

rpois(10000, 150) |>
  hist(breaks=50)

rbinom(1, 150, .92) |>
  hist(breaks=50)

scenarioParameters <- list(
  capacity     = 134, # passengers
  ticket_prc   = 314,
  transf_fee   = 60,
  bumped_cost  = 400  
)

simulateBoarding <- function(overbook_lim=0, demand_avg=150, showup_rate=.92, simParams, n=10000){
  # compound the simulation parameters
  tibble( overbook_lim = overbook_lim,
          demand_avg   = demand_avg,
          showup_rate  = showup_rate ) %>% 
    bind_cols(as_tibble(simParams)) %>%
    # generate distributions
    mutate( actual_demand = list(rpois(n, demand_avg)) ) %>% 
    unnest( actual_demand ) %>% 
    # mutate( read_to_board = map_int( actual_demand, ~rbinom(1,.x,showup_rate) )) %>% 
    # calculate the scenarios outcome
    mutate( booking_capacity = capacity + overbook_lim,
            booked   = map2_dbl(actual_demand, booking_capacity, min),
            # read_to_board = round(showup_rate * booked,0),
            read_to_board = map_int( booked, ~rbinom(1,.x,showup_rate) ),
            no_shows = booked-read_to_board,
            boarding = map2_dbl(capacity, booked, min),
            bumped   = booked-boarding,
            # finance outcomes
            revenue_tickets  = ticket_prc * booked,
            revenue_no_shows = transf_fee * no_shows,
            overbook_cost    = bumped_cost * bumped,
            net_revenue      = revenue_tickets + revenue_no_shows - overbook_cost) %>% 
    return()
}

# booking limit
simulation <- 0:15 %>% 
  map_df(simulateBoarding,simParams=scenarioParameters)


# what overbook limite give us 95% chance to have <= 2 bumped passengers?
simulation %>%
  select(overbook_lim, bumped) %>% 
  nest(bumped) %>% 
  mutate( bumped95pct = map_dbl(data, function(.x){
    .x$bumped %>% 
      quantile(probs=.95) %>% 
      return()
  })) %>% View()
  
simulation %>% 
  filter(overbook_lim==15) %>% 
  pull(bumped) %>% 
  hist()


  group_by(overbook_lim) %>% 
  mutate( bumped95pct = map(bumped, quantile, probs=.95) ) %>% 
  ungroup() %>% 
  ggplot(aes(x=overbook_lim, y=bumped95pct)) +
  geom_point() +
  geom_line() +
  geom_hline(yintercept = 2) +
  theme_minimal()


simulation %>% 
  filter(overbook_lim==2) %>% 
  pull(bumped) %>% 
  quantile() %>% 
  enframe()


