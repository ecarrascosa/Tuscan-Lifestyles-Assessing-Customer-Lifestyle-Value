library(tidyverse)

# parameters of the problem
avg_order <- 104.24
pr_margin <- 0.5
var_cost <- 1

# load data
RFM <- read_csv('rfm.csv')

# Calculate total number of customers who receive the catalogs
total_customers <- sum(RFM$customers)

# or
total_customers <- sum(pull(RFM, customers))

# Calculate total number of customers who ordered from the catalogs
total_buyers <- sum((RFM$buyers))

# Calculate the average response rate
avg_rr <- total_buyers / total_customers

# Calculate the response rate, lift, gain, and profit per cell
(RFM <- mutate(RFM, response_rate = buyers / customers ))

(RFM <- mutate(RFM, lift = response_rate / avg_rr * 100))

(RFM <- mutate(RFM, gain = buyers / total_buyers * 100))

(RFM <- mutate(RFM, profit = buyers * avg_order * pr_margin - var_cost * customers)) # complete the statement

# Question: 
# what is the profit, lift, and gain if we only target 
# the cells with response rate > 1.94%

RR_GT_1_94 <- filter(RFM, response_rate > .0194)

profit_1_94 <- sum(RFM$profit)

rr_1_94 <- sum(RR_GT_1_94$buyers) / sum(RR_GT_1_94$customers)

lift_1_94 <- rr_1_94 / avg_rr * 100

gain_1_94 <- sum(RR_GT_1_94$buyers) / total_buyers * 100


# Calculate the cumulative lift, gain, and profit per cell
# in order to evaluate the performace of targeting

(RFM <- arrange(RFM, desc(response_rate))) # start by ranking the customers

(RFM <- mutate(RFM, cumsum_customer = cumsum(customers))) # cumsum

# Cummulative sum of customers in this ranking, percentile. If you want to target 
# the top 10 percentile of your customer base by response rate,
# you can use this ranking. Similar rankings can be done for profitability or other 
# variables
(RFM <- mutate(RFM, customer_percent_rr = cumsum(customers) / total_customers * 100))

## cumm_rr means that if you target top 10 percentile of customers in the ranking the 
## response rate will be 5.83%, as you grow your target sample it will decrease. 
## If you target the 95K sample the cumm response rate would be 2.45% that we had calculated
(RFM <- mutate(RFM, cumm_rr = cumsum(buyers) / cumsum(customers)))

## cumm_lift means that if you target top 10 percentile of customers in the ranking the 
## response rate will be 5.83%, as you grow your target sample it will decrease. 
## If you target the 95K sample the cumm response rate would be 2.45% that we had calculated
(RFM <- mutate(RFM, cum_lift_rr = cumsum(buyers) / cumsum(customers) / avg_rr * 100))

## cum_gain_rr means that if you target top 10 percentile of customers you
## capture 23% of new buyers that you would get if you would have target the whole sample of 95K
(RFM <- mutate(RFM, cum_gain_rr = cumsum(buyers) / total_buyers * 100)) 

(RFM <- mutate(RFM, cum_profit = cumsum(profit)))

# Top 10% of customers make 51% of profits// Top 25% make 80% of profits
(RFM <- mutate(RFM, profit_percentile = cum_profit / max(cum_profit) * 100))

# Plot the charts

ggplot(RFM) + geom_line(aes(x=customer_percent_rr, y=cum_lift_rr))

ggplot(RFM) + geom_line(aes(x=customer_percent_rr, y=cum_gain_rr))
## If I target top 50% I get 75% of the customer that I get if I target the entire 95K sample.
## If I target top 25 percentile of customers I get close to 50% of all the customers I get if
## I target the entire 95K

ggplot(RFM) + geom_line(aes(x=customer_percent_rr, y=cum_profit))
## This graph shows how much profit you capture by targeting different percentiles of the sample group. 
## If I target top 25 percentile of customers ranked by avg response rate I capture over 30K in profits. 
## If I target the 50 percentile of customers I maximize my profit at close to 40K
## The goal is NOT ALWAYS maximize profit. Sometimes the goal may be to increase
## market share. In that case I sacrifice some profit to increase market
## share by going for a larger percentile even when the profit curve
## gaoes down.

ggplot(RFM) + geom_line(mapping = aes(x = customer_percent_rr, y = profit_percentile ))
