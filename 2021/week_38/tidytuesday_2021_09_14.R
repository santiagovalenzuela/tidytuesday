rm(list=ls(all=T))

library(lubridate)
library(showtext)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-09-14')
billboard <- tuesdata$billboard
audio <- tuesdata$audio_features

