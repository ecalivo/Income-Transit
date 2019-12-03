library(sf)
library(fs)
library(shiny)
library(rgdal)
library(rvest)
library(plotly)
library(janitor)
library(leaflet)
library(GISTools)
library(tidyverse)

options(scipen = 999)

bay_counties <- c("MARIN", 
                  "SONOMA", 
                  "NAPA", 
                  "SOLANO",
                  "CONTRA COSTA",
                  "ALAMEDA",
                  "SANTA CLARA",
                  "SAN MATEO",
                  "SAN FRANCISCO")

city_stops <- all_transit_stops %>% 
  mutate_all(.funs = toupper) %>% 
  group_by(sp_citynam) %>% 
  count() 

municipalities <- city_stops$sp_citynam

url <- "https://en.wikipedia.org/wiki/List_of_California_locations_by_income#Places"

wiki_table <- 
  url %>% 
  read_html() %>% 
  html_nodes(xpath = "//*[@id='mw-content-text']/div/table[3]") %>% 
  html_table()

wiki_tibble <- wiki_table %>% 
  as_tibble(.name_repair = "universal") %>% 
  clean_names()

wiki_tibble <- wiki_tibble$x1 %>% 
  clean_names()

wiki_tibble <- wiki_tibble %>% 
  mutate_all(.funs = toupper) %>% 
  filter(place %in% municipalities,
         county_ies_note_2 %in% bay_counties) 

wiki_tibble <- wiki_tibble[!(wiki_tibble$median_household_income_6 == "[7]"),]

wiki_tibble$med_income <- gsub(",", "", as.character(wiki_tibble$median_household_income_6))

wiki_tibble$med_income <- as.numeric(gsub("\\$", "", wiki_tibble$med_income))   

wiki_tibble$population <- as.numeric(gsub(",", "", as.character(wiki_tibble$population_1)))

municipalities <- wiki_tibble$place 

city_stops <- city_stops %>% 
  filter(sp_citynam %in% municipalities)

the_table <- inner_join(city_stops, wiki_tibble, by = c("sp_citynam" = "place")) 

adjusted <- the_table$n / the_table$population

the_table$adjusted <- adjusted

the_table$county_ies_note_2 <- as.factor(the_table$county_ies_note_2)

write.csv(the_table,"TransitAndIncome/the_table.csv", row.names = TRUE)

the_adjusted_income_plot <- the_table %>% 
  ggplot(aes(x = adjusted, y = med_income, color = county_ies_note_2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  theme(legend.position = "right") +
  labs(title = "Median Household Income Based on Access to Public Transit, Adjusted",
       subtitle = "Adjustment = total transit stops in a municipality divided by population",
       x = "Adjusted Access to Public Transportation (Log Scale)",
       y = "Median Household Income",
       color = "County") 
the_adjusted_income_plotly <- ggplotly(the_adjusted_income_plot)
the_adjusted_income_plotly

the_nonadjusted_income_plot <- the_table %>% 
  ggplot(aes(x = n, y = med_income, color = county_ies_note_2)) +
  geom_point(alpha = 0.5) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_x_log10() +
  theme(legend.position = "right") +
  labs(title = "Median Household Income Based on Transit Stop Numbers",
       subtitle = "Not adjusted for population",
       x = "Number of Rapid Transit Stops in a Municipality",
       y = "Median Household Income")
the_nonadjusted_income_plotly <- ggplotly(the_nonadjusted_income_plot)
the_nonadjusted_income_plotly



