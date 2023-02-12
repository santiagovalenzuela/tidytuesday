rm(list=ls(all=T))

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2023, week = 6)

prices <- tuesdata$big_tech_stock_prices
companies <- tuesdata$big_tech_companies 

prices <- prices %>%
  filter(date > "2019-12-31") %>%
  select(stock_symbol, date, adj_close) %>%
  pivot_wider(names_from = stock_symbol, values_from = adj_close) %>%
  select(date, META, AAPL, AMZN, NFLX, GOOGL)

# We calculate the baseline (01/02/20)
baseline <- prices %>%
  filter(date == "2020-01-02") %>%
  select(META, AAPL, AMZN, NFLX, GOOGL)
  
# We create a new column to show the percentage change compared to the baseline

prices <-prices %>%
  mutate(
    pc_META =((META-baseline$META)/baseline$META)*100,
    pc_AAPL =((AAPL-baseline$AAPL)/baseline$AAPL)*100,
    pc_AMZN =((AMZN-baseline$AMZN)/baseline$AMZN)*100,
    pc_NFLX =((NFLX-baseline$NFLX)/baseline$NFLX)*100,
    pc_GOOGL =((GOOGL-baseline$GOOGL)/baseline$GOOGL)*100,
    ) %>% head()
