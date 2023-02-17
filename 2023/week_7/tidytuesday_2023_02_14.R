rm(list=ls(all=T))

library(tidyverse)
library(tidyquant)
library(showtext)

font_add_google(name = "Lobster", family = "lobster")

tuesdata <- tidytuesdayR::tt_load(2023, week=7)

agaps <-tuesdata$age_gaps

showtext_auto()

age_gaps <- agaps %>% 
  ggplot(aes(x = release_year, y = age_difference)) +
  #geom_boxplot(aes(x = release_year, y = age_difference), group = release_year)) +
  #geom_jitter(aes(x = release_year, y = age_difference, group = release_year)) +
  #geom_smooth(aes(x = release_year, y = age_difference), method = "lm") +
  geom_jitter(color = "#75f5e6",
                  alpha = 0.4) +
  
  geom_smooth(method = "lm",
              color = "white",
              se = FALSE,
              linewidth = 2) +
  
  geom_smooth(method = "lm",
              color = "#f5757c",
              se = FALSE) +
  
  labs(title = "(Movie) Love knows no age?",
       subtitle = "The average age gap between romantic partners in movies has gone down through time. Each point \nrepresents a love interest in a movie",
       x = "Movie release year",
       y = "Age difference") +
  theme_minimal() +
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#f5757c",
                                  family = "lobster"),
        plot.subtitle = element_text(color = "grey"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        axis.title.x = element_text(colour = "#f5757c", family = "lobster"),
        axis.title.y = element_text(colour = "#f5757c", family = "lobster"))

# Save
ggsave(filename = "tt_plot_20230214.png",
       plot = age_gaps,
       device = png,
       dpi = 72,
       width = 624, height = 484, units = "px")
