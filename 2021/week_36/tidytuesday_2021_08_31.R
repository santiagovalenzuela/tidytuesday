rm(list=ls(all=T))

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-08-31')
bird_baths <- tuesdata$bird_baths