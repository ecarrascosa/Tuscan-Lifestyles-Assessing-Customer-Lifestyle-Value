library(tidyverse)

# parameters of the problem
avg_order <- 104.24
pr_margin <- 0.5
var_cost <- 1

# load data
RFM <- read_csv('rfm.csv')

# Calculate total numebr of customers who receive the catalogs
total_customers <- sum(RFM$customers)
# or
total_customers <- sum(pull(RFM, customers))

# Calculate total numebr of customers who ordered from the catalogs
total_buyers <- sum(RFM$buyers)
# or
total_buyers <- sum(pull(RFM, buyers))


# Calculate the average response rate
avg_rr <- total_responses / total_customers
# or 
avg_rr <- sum(RFM$buyers) / sum(RFM$customers)
# or
avg_rr <- sum(pull(RFM, buyers)) / sum(pull(RFM, customers))

# Calculate the response rate, lift, gain, and profit per cell
RFM <- mutate(RFM, response_rate = buyers / customers)

RFM <- mutate(RFM, lift = response_rate / avg_rr * 100)

RFM <- mutate(RFM, gain = buyers / total_buyers * 100)

RFM <- mutate(RFM, profit = buyers * avg_order * pr_margin - customers * var_cost)

# Question: 
# what is the profit, lift, and gain if we only target 
# the cells with response rate > 1.94%
RR_GT_1_94 <- filter(RFM, response_rate > 0.0194)

profit_1_94 <- sum(RR_GT_1_94$profit)

rr_1_94 <- sum(RR_GT_1_94$buyers) / sum(RR_GT_1_94$customers)

lift_1_94 <- rr_1_94 / avg_rr * 100

gain_1_94 <- sum(RR_GT_1_94$buyers) / total_buyers * 100


# Calculate the cumulative lift, gain, and profit per cell
# in order to evaluate the performace of targeting
RFM <- arrange(RFM, desc(response_rate)) # start by ranking the customers

RFM <- mutate(RFM, customer_percent_rr = cumsum(customers) / total_customers * 100)

RFM <- mutate(RFM, cum_lift_rr = cumsum(buyers) / cumsum(customers) / avg_rr * 100)

RFM <- mutate(RFM, cum_gain_rr = cumsum(buyers) / total_buyers * 100)

RFM <- mutate(RFM, cum_profit = cumsum(profit))


# Plot the charts
ggplot(RFM) + geom_line(aes(x=customer_percentile, y=cum_lift))

ggplot(RFM) + geom_line(aes(x=customer_percentile, y=cum_gain))

ggplot(RFM) + geom_line(aes(x=customer_percentile, y=cum_profit))
