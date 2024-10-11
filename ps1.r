rm(list=ls())

####################
## Economic History - Assignment 1
####################



setwd("C:/Users/etien/OneDrive/Documents/GitHub/Econ-History")

  
    ## 2. Open the datasets
library(openxlsx)

car = read.xlsx("StateNewCarRegistrations.xlsx")
stock = read.xlsx("stock_income.xlsx")

head(stock)
head(car)



    ##3.
#a) Merge the datasets: dta3

library(tidyr)
library(dplyr)

car_long <- pivot_longer(car, 
                               cols = -c(month, year), 
                               names_to = "state", 
                               values_to = "car_sales")
head(car_long)

dta3 <- merge(stock, car_long, by = "state", all.x = TRUE)%>%
  filter(!is.na(year)) %>%
  group_by(state)%>%
  mutate(month = as.numeric(month)) %>%
  arrange(year, month, .by_group = TRUE)

head(dta3)
    


    ##4. Create a post-crash dummy: crash

dta4 <- dta3 %>%
  mutate (crash = ifelse(year < 1929 | (year == 1929 & month <= 10), 0, 1))



    ##5. In an ideal world, what should xs be if we're interested in the wealth 
    #channel of the crash?
# The xs should capture the direct impact of the exposure to the stock market 
# crash at the state level on consumption, being an appropriate measure
# of the exogenous variation in stock market exposure at the state level. 
# This variable would need to reflect the pre-crash financial exposure of each 
# state in a way that captures potential heterogeneous treatment effects from 
# the crash, but is also exogenous to consumption trends.
#
# In other words, there must be pre-treatment exogeneity; xs must reflect
# exposure before the crash.
# 
#
# Interesting proxys could be stock ownership rates at the state level or state
# level wealth tied to the financial market.

    ##6. Construct x and explain the idea behind this measure:
dta6 <- dta4 %>%
  mutate (x = dividend_income / total_income)

summary(dta6$x)

# The idea behind this measure is to estimate the exposure of income to the
# stock market crash  by computing the share of total income due to dividends.
# In other words, it should reflect the share of agents' income that depends on 
# the stockmarket since the dividends corresponds to the reward of holdings assets.
# Thus, fluctuations in the stock market will affect dividends, which will in turn
# affect income and consumption.



  ##7. Explain the idea behind regression (1), what are the identification concerns?

# Reg (1): studies effect of stock market crash on consumption at state level by
# using state-level car sales as a proxy.
# $x_s$ (independent variable): captures the exposure of state incomes to 
# fluctuations in the stock market; computed by dividing the proportion of total
# income from dividends in the state by the state total incomes.
# The interaction term enables to examine whether the relationship between the target
# parameter (consumption) and the independent variable (exposure to the stock
# market) changes with the stock market crash.
# Here, beta coefficient is the difference in slope between exposure before and 
# after the crash. In terms of interpretation, if the coefficient is different
# from zero, then we can assume had an impact on the way exposure of income
# to the stock market affected consumption.
#
#
# A time fixed-effect is added to control for potential time trends, while the
# state fixed-effect should control for potential characteristics
# that are specific to states, that we do not observe.
# These fixed-effects are designed to eliminate omitted variable bias by 
# excluding unobserved variables that evolve over time or are specific
# to some states but are constant across entities.
#
#Identification concerns: endogeneity (check Romer)
# Exposure may not be constant over the analysed period.


  
    ##8. 

dta8 = dta6 %>%
  filter(year %in% c(1929, 1930)) %>%
  mutate (log_car = log(car_sales)) %>%
  mutate (time = as.Date(paste(year, as.numeric(month), "01", sep = "-")))
library(lfe)
library(fixest)

#This is with interaction and time fixed effect
reg8_felm = felm(log_car ~ x*crash | time + state, data = dta8)
summary(reg8_felm)

reg8_feols = feols(log_car ~ x*crash | time + state, data = dta8)
summary(reg8_feols)


#This is with interaction
reg8 = felm(log_car ~ x*crash | time + state, data = dta8)
summary(reg8)
#This is without interaction
dta8_bis = dta6 %>%
  filter(year %in% c(1929, 1930)) %>%
  mutate (log_car = log(car_sales)) %>%
  mutate (x_d = x * crash)
reg8_bis = felm(log_car ~ x_d | year + state, data = dta8_bis)
summary(reg8_bis)

    ##9.


    ##10.

    ##11.

