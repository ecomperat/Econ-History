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



    ##5. In an ideal world, what should xs be if we're interested in the wealth channel of the crash?
# ...



    ##6. Construct x and explain the idea behind this measure:
dta6 <- dta4 %>%
  mutate (x = dividend_income / total_income)

summary(dta6$x)

# ...



  ##7. Explain the idea behind regression (1), what are the identification concerns?
# ...


  
    ##8. 

dta8 = dta6 %>%
  filter(year %in% c(1929, 1930)) %>%
  mutate (log_car = log(car_sales))

library(lfe)

#This is with interaction
reg8 = felm(log_car ~ x*crash | year + state, data = dta8)
summary(reg8)
#This is without interaction
dta8_bis = dta6 %>%
  filter(year %in% c(1929, 1930)) %>%
  mutate (log_car = log(car_sales)) %>%
  mutate (x_d = x * crash)
reg8_bis = felm(log_car ~ x_d | year + state, data = dta8_bis)
summary(reg8_bis)

#...

    ##9.


    ##10.

    ##11.

