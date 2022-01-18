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