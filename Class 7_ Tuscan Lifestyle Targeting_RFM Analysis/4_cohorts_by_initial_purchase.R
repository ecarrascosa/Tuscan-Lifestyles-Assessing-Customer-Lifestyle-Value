library(tidyverse)

# parameters of the problem
gross_margin <- .42
cost_pr_year <- 8 * .75
dr <- .1

# load data
COHORTS <- read_csv('cohorts_order_history.csv')

# Calculating profits
COHORTS <- mutate(COHORTS, orders = order_count * frequency)

COHORTS <- mutate(COHORTS, revenue = orders * avg_amount) # complete the statement

COHORTS <- mutate(COHORTS, catalog_costs = cost_pr_year * frequency)

COHORTS <- mutate(COHORTS, profits = revenue * gross_margin - catalog_costs) # complete the statement

# Calculating yearly profits

PROFIT_YEARLY <- COHORTS %>% ## This is the same as 
  group_by(initial, year) %>%
  summarize(customers = sum(frequency), profits = sum(profits)) # complete the statement

# This 
PROFIT_YEARLY <- summarize(group_by(COHORTS,initial, year), customers = sum(frequency), profits = sum(profits))

# Calculating present value of yearly profits
PROFIT_YEARLY <- mutate(PROFIT_YEARLY, pv_profits = profits / (1 + dr) ^ year)

# Calculating present value of yearly profits per cus
PROFIT_YEARLY <- mutate(PROFIT_YEARLY, pv_profits_pr_cust = pv_profits / customers)

# Calculate present value of the profits for each group (50+ and 50-)
PROFIT_YEARLY %>%
  group_by(initial) %>%
  summarize(
    npv_profits = sum(pv_profits),        # complete the statement
    npv_profits_pr_cust = sum(pv_profits_pr_cust) # complete the statement
  )    
