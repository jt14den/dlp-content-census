library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)

dlp_type_year <- read_csv('dlp-content-census.csv')
dlp_durations <- read_csv('derv-durations.csv')
dlp_billions <- read_csv('derv-billion.csv')
dlp_top_level <- read_csv('top_level_tally.csv')

glimpse(dlp_type_year)

# The palette with grey:
cbPalette <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
# added first two primary colors for ucla identity
uclaPalette <- c("#2D68C4", "#F2A900", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

# take file counts by type & year - reshape and plot
dlp_type_year %>% 
  select(type, `2017`, `2018`) %>% 
  gather(year, count, -type) %>% 
  ggplot(aes(x=reorder(type, count), y=count, fill=year)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(y = "File Count") +
    labs(x = "Content type") +
    labs(title= "Content items by type") +
    scale_fill_manual(values=uclaPalette) + 
    theme_minimal()

dlp_durations %>% 
  gather(year, duration, -DerivedStats_durations_hours) %>% 
  mutate(days = duration/24) %>% 
  ggplot(aes(x = DerivedStats_durations_hours, y = duration, fill=year)) +
    geom_col(position="dodge") +
    coord_flip() +
    labs(y = "Duration in Hours", x = "Type", fill = "Year") +
    labs(title = "Derived Stats: Duration in Hours by Type") +
    scale_y_continuous(labels = comma) +
    scale_fill_manual(values=uclaPalette) +
    theme_minimal()

dlp_billions %>% 
  gather(year, counts_billions, -DerivedStats_numcount_billions) %>% 
  mutate(counts_mill = counts_billions*1000) %>% 
  ggplot(aes(x=DerivedStats_numcount_billions, y = counts_billions, fill=year)) +
    geom_col(position = "dodge") +
    coord_flip() +
    labs(y = "Counts in Billions", x = "Type", fill = "Year", title="Derived Counts Estimated") +
    scale_fill_manual(values=uclaPalette) +
    theme_minimal()

dlp_top_level %>% 
  gather(year, counts, -top_level_tally) %>% 
  ggplot(aes(x=reorder(top_level_tally, counts), y = counts, fill = year)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(y = "Counts in Terabytes", x = "Type", fill = "Year", title="Top Level Tally") +
  scale_fill_manual(values=uclaPalette) +
  theme_minimal()
  
  
