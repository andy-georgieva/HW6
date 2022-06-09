# https://dplyr.tidyverse.org/articles/dplyr.html - the basic
# https://dplyr.tidyverse.org/articles/grouping.html - group by in details
# https://dplyr.tidyverse.org/articles/base.html - comparing dplyr functions
# to the base R function.

library(RcppRoll)
library(tidyverse)
library(tidyquant)
#####Problem 1#####
# Download data for a stock of your choice and do the following:
# Calculate the 20 day SMA of the stock price and define upper and
# lower bounds around it which are equal to SMA +-2 standard deviation
# the past observations used to calculate the SMA.
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the price goes above the upper bound - sell.
# If the price goes below the lower bound - buy. 

stock <- tidyquant::tq_get("AAPL") %>% 
  select(symbol, date, adjusted) %>% 
  mutate(SMA20 = SMA(adjusted, n=20),
         SD20 = RcppRoll::roll_sd(adjusted, n=20, fill = NA, align = "right")) %>% 
  filter(!is.na(SMA20)) 

bounds <- stock %>% 
  mutate(upper_bound = SMA20 + SD20*2,
         lower_bound = SMA20 - SD20*2, 
         action = case_when(adjusted > upper_bound ~ "sell",
                             adjusted < lower_bound ~ "buy", 
                             TRUE ~ "indifferent"))


#####Problem 1#####

#####Problem 2#####
# Calculate the RSI using the instruction about the formula from here:
# https://www.investopedia.com/terms/r/rsi.asp
# Employ the following strategy and compare to a baseline strategy of buy and hold:
# If the RSI above 65 - sell.
# If the price goes below 35 - buy. 

library(TTR)
comparison <- stock %>% 
  mutate(RSI = RSI(adjusted, n=14)) %>% 
  filter(!is.na(RSI)) %>% 
  mutate(compare = case_when(RSI > 65 ~ "sell",
                             adjusted < 35 ~ "buy",
                             TRUE ~ "no matter"))

#####Problem 2#####