## load libraries
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(lubridate)
theme_set(theme_bw(base_size = 16, base_family = "mono"))

# fetch data
tdf_winners <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-04-07/tdf_winners.csv')

# prep for plot and plot it
tdf_winners %>% 
  mutate(`Minutes Back` = 60 * time_margin) %>% 
  filter(Year > 1949, !is.na(`Minutes Back`))%>% 
  ggplot(aes(Year, distance)) +
  geom_point(aes(color = `Minutes Back`), size = 3) +
  scale_color_viridis_c(option = "D") +
  ylab("Total Race Distance (km)") +
  xlab("Race Year") +
  labs(
    title = "Le Tour de France: shorter races & closer finishes",
    subtitle = "total distance covered by year and gap from 1st to 2nd place",
    caption ="@PedalAaronOakes #TidyTuesday 2020-04-07 \n Data:  https://github.com/alastairrushworth/tdf"
  ) +
  theme( panel.grid.minor = element_blank(),
         axis.title.x = element_text(margin = unit(c(5, 0, 0, 0), "mm")),
         axis.title.y = element_text(margin = unit(c(0, 5, 0, 0), "mm"))) +
  ylim(3000, 5000) +
  xlim(1949, 2020) 

