rm(list=ls(all=T))

library(tidytext)
library(tidyverse)
library(showtext)

font_add_google(name = "Staatliches", family = "staatliches")

# Load data
papers <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-09-28/papers.csv')
data("stop_words")

# We tokenize the titles, count how many times each word is repeated,
# remove the stopwords and create a new, filtered tibble with the most
# common words

econowords <- papers %>%
  unnest_tokens(word, title) %>% 
  group_by(word) %>%
  tally() %>%
  arrange(desc(n)) %>%
  anti_join(stop_words) %>%
  head(n=15)

# Plot data
showtext_auto()

econoplot <-econowords %>% ggplot(aes(fct_reorder(word,n), n)) +
  geom_col(fill = "#000099") +
  labs(title = "Economists like to write about evidence",
       subtitle = "Most common words in the title of NBER's working papers, 1973-2021",
       caption = "Source: National Bureau of Economic Research (NBER), by way of the nberwp package by Ben Davies.") +
  coord_flip(expand = F) +
  theme_minimal() +
  theme(axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        plot.title.position = "plot",
        plot.title = element_text(color = "#000099"),
        plot.caption.position = "plot",
        plot.caption=element_text(hjust = 0),
        text = element_text(family = "staatliches"))

ggsave(filename = "tt_plot_20210928.png",
       plot = econoplot,
       device = "png",
       dpi = 72,
       width = 800, height= 380, units = "px")
