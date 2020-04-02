## Tidy Tuesday 2020-03-31
library(tidyverse)

# import the data
brewing_materials <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewing_materials.csv')
beer_taxed <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_taxed.csv')
brewer_size <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/brewer_size.csv')
beer_states <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2020/2020-03-31/beer_states.csv')

#According to a [source](https://en.wikipedia.org/wiki/Craft_brewery_and_microbrewery) [or two](https://www.brewersassociation.org/statistics-and-data/craft-beer-industry-market-segments/) a microbrewery is one that produces fewer than 15,000 barrel annually.  
#I might be interested to see what proportion of the beer production is done by microbreweries.

brewer_size %>%
  filter(brewer_size != "Total") %>%
  mutate(
    Year = factor(year),
    `Brewer Size`= fct_collapse(
      brewer_size,
      Micro = c(
        "7,501 to 15,000 Barrels",
        "1,001 to 7,500 Barrels" ,
        "1 to 1,000 Barrels",
        "Under 1 Barrel",
        "Zero Barrels"
      ),
      Regional = c(
        "15,001 to 30,000 Barrels",
        "30,001 to 60,000 Barrels",
        "60,001 to 100,000 Barrels",
        "100,001 to 500,000 Barrels",
        "500,001 to 1,000,000 Barrels",
        "1,000,001 to 6,000,000 Barrels",
        "1,000,000 to 6,000,000 Barrels",
        "1,000,001 to 1,999,999 Barrels",
        "2,000,000 to 6,000,000 Barrels"
      ),
      Large = c("6,000,001 Barrels and Over")
    )
  ) %>%
  group_by(`Brewer Size`, Year) %>%
  summarize(`Total Barrels` = sum(total_barrels, na.rm = TRUE) / 1e6) %>%
  ggplot(aes(x = Year, y = `Total Barrels`)) + 
  geom_col(aes(fill = `Brewer Size`), show.legend = FALSE) +
  scale_fill_viridis_d(option = "E") +
  facet_wrap(~`Brewer Size`, strip.position = "top",  scales = "free_y", ncol = 1) +
  theme_bw(base_size = 14, 
                base_family = "mono") +
  scale_y_continuous(name = "Barrels Produced (million)") +
  labs(
    title = "Are Microbreweries on the Rise?",
    subtitle = "total annual beer production by brewery size",
    caption = "@PedalAaronOakes #TidyTuesday 2020-03-31 \n Data:  Alcohol and Tobacco Tax and Trade Bureau"
  )

