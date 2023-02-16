rm(list=ls(all=T))

library(tidyverse)

tuesdata <- tidytuesdayR::tt_load(2023, week=7)

agaps <-tuesdata$age_gaps
