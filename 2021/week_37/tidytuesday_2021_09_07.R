rm(list=ls(all=T))

library(ggdist)
library(patchwork)
library(showtext)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-09-07')

showtext_auto()

# Data cleansing: we create dfs with the data we are going to need

results_df <- tuesdata$results

drivers_df <-tuesdata$drivers %>%
  select(driverId, driverRef, number, code,
         forename, surname, nationality) %>%
  rename(driverNumber = number)

race_df <- tuesdata$races %>%
  select(raceId, year, round, circuitId, name, date)

new_df <- left_join(results_df, drivers_df, by = "driverId")
new_df <-left_join(new_df, race_df, by = "raceId")


# First graph: a heatmap showing the GP's where a Mexican driver has competed

new_df <- new_df %>%
  mutate(year = as.numeric(year),
         mx_raced =case_when(
           nationality == "Mexican" ~ TRUE,
           nationality != "Mexican" ~ FALSE)
  )



g1 <-new_df %>%
  select(year, mx_raced) %>%
  group_by(year) %>%
  summarise_all(max) %>%
  ggplot( aes(x=year, y = year, fill = mx_raced)) +
  geom_tile()

# Second graph: distribution of standings of Mexican drivers

g2 <- new_df %>%
  filter(nationality == "Mexican") %>%
  ggplot(aes(x = positionOrder, fill = "red", stroke = 0)) +
  stat_dots() +
  #coord_flip(expand = F, clip = "off") +
  #scale_x_reverse() +
  coord_cartesian(expand = F, clip = "off") +
  theme_minimal()