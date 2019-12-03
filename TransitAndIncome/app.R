#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(sf)
library(fs)
library(shiny)
library(rgdal)
library(rvest)
library(plotly)
library(janitor)
library(leaflet)
library(GISTools)
library(shinythemes)
library(tidyverse)

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("darkly"),
                navbarPage("Public Transportation & Income in the San Francisco Bay Area",
                           tabPanel("About", HTML('<center><img src = "https://upload.wikimedia.org/wikipedia/commons/d/d7/San_Francisco_Bay_Bridge_Western_Span_at_night.jpg",
                                                  width = "100%", height = "100%"></center>'),
                                    p("I grew up in San Jose, at the southern end of the San Francisco Bay Area. Growing up, both of my parents worked for public transportation companies, which meant
                                      that I became familiar with taking public buses and trains pretty early on in my childhood. While I thought the idea of intricate networks of interconnectedness were
                                      fascinating and exciting, I quickly learned that many people did not feel the same way. Instead, public transportation was thought of as inefficient, unsanitary, and 
                                      associated with poverty and low socioeconomic status."),
                                    p("This is a view that is commonly taken across the United States, and especially in the car-happy state of California. However, it is one that is rapidly changing. 
                                      Given the rapid population growth in the area combined with movements to exurbs and towns on the Bay's periphery, one of these areas is public transportation. A Bay 
                                      Area that moves faster and more efficiently is in the interest of all residents, providing a path towards decreased dependencies on cars, greater regional cooperation in 
                                      one of the nation's most important megalopolises, and encouraging urban design that is heavily focused on walkability, productive interpersonal interaction, and environmental sustainability."),
                                    p("This is an issue that I've thought about almost my entire life: both of my parents worked for public transit agencies during my childhood, giving me a somewhat unique insight into the ways that 
                                      certain populations rely on transit to get around. Here at Harvard, I have pursued urban studies through the Government and History of Art and Architecture departments, allowing me to learn more 
                                      about how urban infrastructure, including public transit, shape the way that people live, work, and thrive."),
                                    p("In order to improve these infrastructures, we have to know how they currently work -- and don't work. Enter this project, which was created by Emmanuel Calivo (eacalivo@gmail.com). I took data from 
                                      the open data portal for the Santa Clara Valley Transportation Authority (VTA), as well as the government of Santa Clara County and the Census Brueau, in order to see if there is a relationship between
                                      access to public transit and median income levels.")
                                    ),
                           tabPanel("Visualization",
                                    h2("Interactive Map of Bay Area Transit Stops"),
                                    p("This map allows you to examine the 23,000+ rapid transit stops across the Bay Area, with overlays to show you municipal boundaries. 
                                       Zoom in to see exact stop locations."),
                                    leafletOutput("map", height = 600)
                           ),
                           tabPanel("Data & Model",
                                    sidebarLayout(
                                      sidebarPanel(
                                                   selectInput("x",
                                                               label = "Adjust for population?",
                                                               choices = c("Adjusted",
                                                                           "Not Adjusted"),
                                                               selected = "Adjusted"),
                                                   checkboxInput("county",
                                                                 label = "Show Stops By County?",
                                                                 value = FALSE)),
                                      mainPanel(plotlyOutput("plots"))))))

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% 
      addProviderTiles("Stamen.Terrain") %>% 
      addPolygons(data = juris_bay,
                  color = "black",
                  weight = 1.5,
                  opacity = 1,
                  fillColor = "white",
                  fillOpacity = 0.5,
                  highlightOptions = highlightOptions(color = "red")
      ) %>%
      addCircleMarkers(data = transit_clean,
                       radius = 4,
                       clusterOptions = markerClusterOptions(iconCreateFunction =
                                                               JS("
                                                                  function(cluster) {
                                                                  return new L.DivIcon({
                                                                  html: '<div style=\"background-color:rgba(0, 180, 0, 1)\"><span>' + cluster.getChildCount() + '</div><span>',
                                                                  className: 'marker-cluster'
                                                                  });
                                                                  }"), maxClusterRadius = 100)
  )
  })
  
  output$plots <- renderPlotly({
    
    if(input$county == "TRUE") {
      
      if(input$x == "Adjusted") {
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
      }
      
      else
        
      {
        the_nonadjusted_income_plot <- the_table %>% 
          ggplot(aes(x = n, y = med_income, color = county_ies_note_2)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_x_log10() +
          theme(legend.position = "right") +
          labs(title = "Median Household Income Based on Transit Stop Numbers",
               subtitle = "Not adjusted for population",
               x = "Number of Rapid Transit Stops in a Municipality",
               y = "Median Household Income",
               color = "County")
        the_nonadjusted_income_plotly <- ggplotly(the_nonadjusted_income_plot)
        the_nonadjusted_income_plotly
      }
    }
    
    else {
      if(input$x == "Adjusted") {
        the_adjusted_income_plot_2 <- the_table %>% 
          ggplot(aes(x = adjusted, y = med_income)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_x_log10() +
          labs(title = "Median Household Income Based on Access to Public Transit, Adjusted",
               subtitle = "Adjustment = total transit stops in a municipality divided by population",
               x = "Adjusted Access to Public Transportation (Log Scale)",
               y = "Median Household Income") 
        the_adjusted_income_plotly_2 <- ggplotly(the_adjusted_income_plot_2)
        the_adjusted_income_plotly_2
      }
      else
        {
        the_nonadjusted_income_plot <- the_table %>% 
          ggplot(aes(x = n, y = med_income)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_x_log10() +
          labs(title = "Median Household Income Based on Transit Stop Numbers",
               subtitle = "Not adjusted for population",
               x = "Number of Rapid Transit Stops in a Municipality",
               y = "Median Household Income")
        the_nonadjusted_income_plotly <- ggplotly(the_nonadjusted_income_plot)
        the_nonadjusted_income_plotly
      }
     
    }
   
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


