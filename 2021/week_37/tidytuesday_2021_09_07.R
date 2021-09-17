rm(list=ls(all=T))

library(ggdist)
library(patchwork)
library(showtext)
library(tidyverse)

tuesdata <- tidytuesdayR::tt_load('2021-09-07')

#font_add_google("Titillium Web", "titillium")
showtext_auto()

# Data cleansing

results_df <- tuesdata$results

drivers_df <-tuesdata$drivers %>%
  select(driverId, driverRef, number, code,
         forename, surname, nationality) %>%
  rename(driverNumber = number)

race_df <- tuesdata$races %>%
  select(raceId, year, round, circuitId, name, date)

new_df <- left_join(results_df, drivers_df, by = "driverId")
new_df <-left_join(new_df, race_df, by = "raceId")

# We give a name to the color we are going to use:
red <- "#FF1801"

################################################################################
# First graph: a heatmap showing the GP's where a Mexican driver has competed
################################################################################


# We create a grid with all the years that have had F1 races
year_grid <- expand.grid(years = 0:9,
                         decades = seq(from = 1950, to= 2020, by = 10)
            )

year_grid <- year_grid %>%
  mutate(year = years +decades)

# We create a new df with the years a Mexican driver has raced, and then join it
# to the year grid we created earlier
new_df <- new_df %>%
  mutate(year = as.numeric(year),
         mx_raced =case_when(
           nationality == "Mexican" ~ TRUE,
           nationality != "Mexican" ~ FALSE)
  )

mx_years <-new_df %>%
  select(year, mx_raced) %>%
  group_by(year) %>%
  summarise_all(max)

year_grid <-left_join(year_grid, mx_years, by = "year")
year_grid$mx_raced <- as.factor(year_grid$mx_raced)
year_grid$years <- as.factor(year_grid$years)

year_grid %>%
  ggplot(aes(x = fct_rev(as_factor(decades)), y = years, fill = mx_raced)) +
  geom_tile(colour="white", size=0.25) + 
  coord_flip(expand = F) +
  scale_fill_manual(values=c("black", red))+
  labs(x = NULL, y = NULL) +
  theme(legend.position = "None")

################################################################
# Second graph: distribution of standings of Mexican drivers
################################################################

g2 <- new_df %>%
  filter(nationality == "Mexican") %>%
  ggplot(aes(x = positionOrder, fill = red, stroke = 0)) +
  stat_dots() +
  #coord_flip(expand = F, clip = "off") +
  #scale_x_reverse() +
  coord_cartesian(expand = F, clip = "off") +
  theme_minimal()+
  labs(y = NULL) +
  theme(legend.position = "None")

########################################################################
# Final steps: joining the graphs into a single plot and saving it
########################################################################