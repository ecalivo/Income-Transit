library(sf)
library(fs)
library(gt)
library(shiny)
library(broom)
library(rgdal)
library(rvest)
library(plotly)
library(janitor)
library(leaflet)
library(GISTools)
library(tidyverse)

options(scipen = 999)

adjusted_model <- lm(data = the_table, adjusted ~ med_income) 

adjusted_table <- tidy(adjusted_model, conf.int = FALSE) %>% 
  select(term, estimate, std.error)

adjusted_final <- adjusted_table %>% 
  gt() %>% 
  tab_header(title = "Regression Model: Adjusted Transit Access Based on Median Household Income",
             subtitle = "The magnitude of change is relatively small.") %>% 
  cols_label(term = "",
             estimate = "Coefficient",
             std.error = "Standard Error") %>% 
  gtsave("TransitAndIncome/adjusted_final.html")


non_adjusted_model <- lm(data = the_table, n ~ med_income)

non_adjusted_table <- tidy(non_adjusted_model, conf.int = FALSE) %>% 
  select(term, estimate, std.error)

non_adjusted_final <- non_adjusted_table %>%  
  gt() %>% 
  tab_header(title = "Regression Model: Non-Adjusted Transit Access Based on Median Household Income",
             subtitle = "The magnitude of change is still relatively small.") %>% 
  cols_label(term = "",
             estimate = "Coefficient",
             std.error = "Standard Error")
non_adjusted_final
