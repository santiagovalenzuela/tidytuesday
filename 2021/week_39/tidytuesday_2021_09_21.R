rm(list=ls(all=T))

library(gganimate)
library(showtext)
library(tidyverse)

#font_add_google("Carrois Gothic SC", "carrois_gothic")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-09-21')
nominees <- tuesdata$nominees

distributors <- c("Netflix", "HBO")

dist_df <-nominees %>% 
  select(distributor, year) %>%
  group_by(distributor, year) %>%
  tally() %>%
  filter(distributor %in% distributors ) %>%
  arrange(year)

p <- dist_df %>%
  ggplot(aes(x = year, y = n, color = distributor)) +
  geom_line(size = 1.1) +
  geom_point(size = 2) +
  
  labs(x = NULL,
       y = "Number of Emmy nominations",
       color = NULL,
       title = "HBO vs Netflix",
       caption = "Source: emmys.com",
       subtitle =  "Total number of Emmy nominations per distributor, per year \n {trunc(frame_along)}") +
  
  scale_color_manual(values = c("#5822b4", "#E50914")) +
  theme_minimal() +
  
  theme(legend.position= "bottom",
        plot.title = element_text(color = "#5822b4"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust = 0),
        text = element_text(family="carrois_gothic")
        )

anim <- p +
  transition_reveal(year) +
  view_follow()

animate(anim, renderer=gifski_renderer("NetflixHBO.gif"))
