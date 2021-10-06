rm(list=ls(all=T))

library(maps)
library(scales)
library(showtext)
library(tidyverse)

font_add_google(name = "Poppins", family = "popins")

# Load data

tuesdata <- tidytuesdayR::tt_load(2021, week = 41)
nurses <- tuesdata$nurses
states <- map_data("state")

# Clean data

#Convert nurses' column names to lowercase
nurses <- rename_with(nurses, tolower)

#Remove special characters and replace whitespace with underscores
colnames(nurses) <- str_remove_all(colnames(nurses), "[:punct:]")
colnames(nurses) <- str_replace_all(colnames(nurses), "[:blank:]", "_")

#Create a new column with state's name in lowercase so we can join it with the states df
nurses$region <- tolower(nurses$state)

#Filter
tiny_nurses <- nurses %>%
  select(region, year, annual_salary_median) %>%
  filter(year == 2020)

salary_map <- left_join(states, tiny_nurses, by = "region")

# Plot

showtext_auto()

us_map <-ggplot(salary_map, aes(long, lat, group = group,  fill = annual_salary_median)) +
  geom_polygon(color = "white") +
  labs(title = "Nurses in California are the best paid in the United States",
       subtitle = "In 2020, Californian registered nurses earned a median annual salary of $118,410;\nin contrast, Alabaman nurses earned just $58,630",
       caption = "Source: Data uploaded to data.world by Lindsay Betzendahl",
       fill = "Median annual salary",
       x = NULL,
       y = NULL) +
  scale_fill_binned(labels= dollar,
                    low = "#46b8ff", #
                    high = "#215a7d") +
  guides(fill = guide_legend(title.position = "top")) +
  theme_minimal() +
  theme(axis.line = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "bottom",
        legend.title.align = 0.5,
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(color = "#215a7d"),
        plot.title.position = "plot",
        plot.caption.position = "plot",
        plot.caption = element_text(face = "italic", hjust=0),
        text = element_text(family = "popins"))

# Save
ggsave(filename = "tt_plot_20211005.png",
       plot = us_map,
       device = png,
       dpi = 72,
       width = 624, height = 484, units = "px")
