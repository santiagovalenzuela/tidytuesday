rm(list=ls(all=T))

library(lubridate)
library(patchwork)
library(showtext)
library(tidyverse)

#font_add_google("Raleway", "raleway")
showtext_auto()

# Download this week's datasets
tuesdata <- tidytuesdayR::tt_load('2021-09-14')
billboard <- tuesdata$billboard
audio <- tuesdata$audio_features

# Convert week_id to date format
billboard$week_id <- strptime(billboard$week_id, "%m/%d/%Y")
billboard$week_id <-as_date(billboard$week_id)

# We filter and create a new df
n_1s <- billboard %>% filter(week_position == 1) %>% arrange(week_id)

# Join n_1s df to audio df
n_1s <-left_join(n_1s, audio, by ="song_id")

#Loudness
loudness <-n_1s %>%
  ggplot(aes(week_id, loudness)) +
  
  geom_point(color = "white", stroke = 1.5) +
  geom_point(color = "#1DB954") +
  
  geom_smooth(method = "glm",
              se = F,
              color = "white",
              size = 2) +
  geom_smooth(method = "glm",
              se = F,
              color = "#b92a1d") +
  
  coord_cartesian(expand = F, clip = "off") +
  labs(x = NULL,
       y = NULL,
       legends = NULL,
       title = "Loudness",
       subtitle = "In decibels") +
  theme_minimal() +
  theme(legend.position = "None",
        plot.title.position = "plot",
        text = element_text(family = "raleway"),
        plot.title = element_text(color = "#1DB954"))

# Danceability
danceability <-n_1s %>%
  ggplot(aes(week_id, danceability)) +
  
  geom_point(color = "white", stroke = 1.5) +
  geom_point(color = "#b92a1d") +
  
  geom_smooth(method = "glm",
              se = F,
              color = "white",
              size = 2) +
  geom_smooth(method = "glm",
              se = F,
              color = "#1db951") +
  
  coord_cartesian(expand = F, clip = "off") +
  labs(x = NULL,
       y = NULL,
       legends = NULL,
       title = "Danceability",
       subtitle = "How suitable a track is for dancing based on \na combination of musical elements") +
  theme_minimal() +
  theme(legend.position = "None",
        plot.title.position = "plot",
        text = element_text(family = "raleway"),
        plot.title = element_text(color = "#1DB954"))

# Energy
energy <-n_1s %>%
  ggplot(aes(week_id, energy)) +
  
  geom_point(color = "white", stroke = 1.5) +
  geom_point(color = "#b9781d") +
  
  geom_smooth(method = "glm",
              se = F,
              color = "white",
              size = 2) +
  geom_smooth(method = "glm",
              se = F,
              color = "#371db9") +
  
  coord_cartesian(expand = F, clip = "off") +
  labs(x = NULL,
       y = NULL,
       legends = NULL,
       title = "Energy",
       subtitle = "A perceptual measure of intensity \nand activity") +
  theme_minimal() +
  theme(legend.position = "None",
        plot.title.position = "plot",
        text = element_text(family = "raleway"),
        plot.title = element_text(color = "#1DB954"))

# Valence

valence <-n_1s %>%
  ggplot(aes(week_id, valence)) +
  
  geom_point(color = "white", stroke = 1.5, na.rm = T) +
  geom_point(color = "#3a1db9", na.rm = T) +
  
  geom_smooth(method = "glm",
              se = F,
              color = "white",
              size = 2) +
  geom_smooth(method = "glm",
              se = F,
              color = "#b97b1d") +
  
  coord_cartesian(expand = F, clip = "off") +
  labs(x = NULL,
       y = NULL,
       legends = NULL,
       title = "Valence",
       subtitle = "Tracks with high valence sound more positive, while\ntracks with low valence sound more negative") +
  theme_minimal() +
  theme(legend.position = "None",
        plot.title.position = "plot",
        text = element_text(family = "raleway"),
        plot.title = element_text(color = "#1DB954"))

final_plot <-(loudness|energy)/(danceability|valence) +
  plot_annotation(title = "Louder, more energetic, more danceable and more negative",
                  subtitle = "How Billboard's number 1 song has changed throughout time across four audio features (as measured by Spotify)",
                  caption =  "Source: Sean Miller, Billboard.com and Spotify") &
  theme(text = element_text(family = "raleway"),
        plot.title = element_text(color = "#1DB954"),
        plot.caption=element_text(hjust = 0))

ggsave(filename = "tt_plot_20210914.png",
       plot = final_plot,
       device = "png",
       dpi = 72,
       width = 896, height= 467, units = "px")
