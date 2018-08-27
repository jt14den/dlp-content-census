library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

dlp_type_year <- read_csv('dlp-content-census.csv')

glimpse(dlp_type_year)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")



dlp_type_year %>% 
  select(type, `2015`, `2016`, `2017`, `2018`) %>% 
  gather(year, count, -type) %>% 
  ggplot(aes(x=reorder(type, count), y=count, fill=year)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(y = "File Count") +
    labs(x = "Content type") +
    labs(title= "Content items by type") +
    scale_fill_manual(values=cbPalette) + 
    theme_minimal()

