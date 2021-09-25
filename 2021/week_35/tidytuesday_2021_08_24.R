rm(list=ls(all=T))

library(tidyverse)
library(ggdist)
library(showtext)

#font_add_google("Roboto", "Roboto")

lemurs <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-08-24/lemur_data.csv')

df <- lemurs %>% select(birth_type, n_known_offspring)


lemur_plot <- df %>%
  # Remove lemurs ofunknown birth type 
  filter(birth_type != "Unk") %>% 
  ggplot() +
  aes(x = n_known_offspring,
      y = fct_reorder(as.factor(birth_type),
                      n_known_offspring,
                      .fun = 'median',
                      na.rm = T),
      fill = as.factor(birth_type)) +
  stat_halfeye(
    # moves geom up
    justification = -0.2,
    # this removes the slab interval:
    .width = 0 
    ) +
  geom_boxplot(alpha = 0.3,
               outlier.color = NA,
               width = 0.3
               ) +
  scale_fill_brewer(palette = "Accent") +
  theme_ggdist() +
  labs(title = "Lemur offspring",
       subtitle = "Wild-born lemurs (WB) tend to have a larger offspring than those that are captive-born (CB)",
       caption = "Source: Zehr et al (2014) \nSci. Data 1:140019 doi: 10.1038/sdata.2014.19",
       x = "Number of known offspring",
       y = NULL,
       fill = NULL) +
  
  theme(legend.position = "none",
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption=element_text(hjust = 0, family = "Roboto"),
        text = element_text(family = "Roboto"))

ggsave(filename = "tt_plot_20210824.png",
       plot = lemur_plot,
       device = "png",
       dpi = 72,
       width = 896, height= 467, units = "px")
