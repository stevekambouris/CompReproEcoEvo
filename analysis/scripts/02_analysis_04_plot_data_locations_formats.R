# 02_analysis_04_plot_data_locations_formats.R ---------------------------------
#
# Reproducing results from meta-analyses in Ecology and Evolution
#
# This script imports formatted data listing shared data locations, and shared
# data formats, and creates plots for publication.

# Load required libraries.
library(tidyverse)
library(here)
library(ggplot2)



# Import data and perform initial cleaning/formatting --------------------------

# Import the data set containing the locations of the shared data files.
shared_data_locations <- read_csv(file = here("data", "clean",
                                              "shared_data_locations.csv"))

num_articles_loc <- shared_data_locations %>% 
  distinct(SearchID) %>% 
  nrow()

shared_data_loc_forplot <- shared_data_locations %>% 
  group_by(data_location) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct_of_articles = 100*n/num_articles_loc) %>% 
  arrange(pct_of_articles, data_location) %>% 
  mutate(factor_data_loc = factor(data_location, levels = data_location))


# Import the data set containing the formats of the shared data files.
shared_data_formats <- read_csv(file = here("data", "clean",
                                            "shared_data_formats.csv"))

num_articles_fmt <- shared_data_formats %>% 
  distinct(SearchID) %>% 
  nrow()

shared_data_fmt_forplot <- shared_data_formats %>% 
  group_by(format_category) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  mutate(pct_of_articles = 100*n/num_articles_loc) %>% 
  arrange(pct_of_articles, format_category) %>% 
  mutate(factor_data_fmt = factor(format_category, levels = format_category))



# Plot shared data locations ---------------------------------------------------

data_loc_plot <- ggplot(data = shared_data_loc_forplot,
                        mapping = aes(x = factor_data_loc,
                                      y = pct_of_articles)) +
  geom_bar(stat = "identity", colour = "black", fill = "green4") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of meta-analyses (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

#data_loc_plot

ggsave(filename = "figure_data_sharing_locations.pdf",
       plot = data_loc_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)


# Plot shared data formats -----------------------------------------------------

data_fmt_plot <- ggplot(data = shared_data_fmt_forplot,
                        mapping = aes(x = factor_data_fmt,
                                      y = pct_of_articles)) +
  geom_bar(stat = "identity", colour = "black", fill = "#26004d") +
  coord_cartesian(ylim = c(0, 100)) +
  scale_y_continuous(expand = c(0, 0)) +
  labs(x = NULL, y = "Percentage of meta-analyses (%)") +
  coord_flip(ylim = c(0, 100)) +
  theme_bw() +
  theme(plot.margin = unit(c(0.2, 0.5, 0.2, 0.2), "cm"),
        panel.grid.major.y = element_blank())

#data_fmt_plot

ggsave(filename = "figure_data_sharing_formats.pdf",
       plot = data_fmt_plot,
       device = "pdf",
       path = here("analysis", "output"),
       width = 15,
       height = 10,
       units = "cm",
       dpi = 300)
