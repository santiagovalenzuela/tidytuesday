rm(list=ls(all=T))

library(tidytext)
library(tidyverse)
library(showtext)

font_add_google(name = "Balsamiq Sans", family = "balsamiq")
showtext_auto()

tuesdata <- tidytuesdayR::tt_load('2021-08-31')
bb <- tuesdata$bird_baths

# We keep only the top 5 type of birds in each bioregion
b_counts <- bb %>%
  filter(bird_count >0) %>% 
  group_by(bird_type, bioregions) %>%
  summarize(new_bird_count = sum(bird_count)) %>%
  filter(is.na(bioregions) == F) %>%
  arrange(desc(new_bird_count)) %>%
  group_by(bioregions) %>%
  top_n(n=5)

# Plot the data
bird_plot <-b_counts %>%
  ggplot(aes(reorder_within(bird_type, new_bird_count, bioregions),
             new_bird_count,
             fill = bioregions)) +
  geom_col(show.legend = FALSE) +
  facet_wrap( ~bioregions, scales = 'free') +
  coord_flip(expand = F) +
  scale_x_reordered() +
  scale_fill_brewer(palette= "Paired") +
  labs(title = "Most common Australian birds",
       subtitle = "Five most common bird species spotted in Australia (2014-15), by bioregion",
       caption = "Source: Cleary et al, 2016") +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        text = element_text(family = "balsamiq"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust = 0))

ggsave(filename = "tt_plot_20210831.png",
       plot = bird_plot,
       device = "png",
       dpi = 72,
       width = 1193, height= 414, units = "px")
