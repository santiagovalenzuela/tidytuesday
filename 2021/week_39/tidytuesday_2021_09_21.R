rm(list=ls(all=T))

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
nominees <- tuesdata$nominees

nominees %>% 
  select(distributor, year) %>%
  group_by(distributor, year) %>%
  tally() %>%
  View()
