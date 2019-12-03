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
library(gt)
library(broom)
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
                                    p("The San Francisco Bay Area has changed dramatically over the past few decades. As America’s preeminent center for information technology, it has been an incubator for many of the world’s most 
                                      important and innovative tech companies and has welcomed many thousands of new, high-skilled workers to fuel their growth and success. Consequently, the area’s economy is booming: housing prices are 
                                      some of the highest in the nation, and median household income is well above the national and state averages. However, while some have prospered, many more have seen difficulties: as the cost of living in the 
                                      major urban centers and their close suburbs has skyrocketed, many of these middle-to-lower class residents have moved to more affordable exurbs on the region’s periphery, leading to longer commute times and 
                                      high rates of traffic congestion."),
                                    p("Due to these changes, public transportation development is a hot topic in the Bay Area as policymakers, businesses, and residents seek improved methods of moving across an increasingly interconnected region. 
                                      Bay Area Rapid Transit (BART) is currently extending its service south to Silicon Valley, while Facebook has floated the renovation of old and unused transit infrastructure in order to create another flow of 
                                      transport across the Bay. But who do these systems serve? Do the rich and poor alike have access to the Bay’s extensive public transportation network? More specifically, is there a relationship between 
                                      household income and access to public transit?"),
                                    p("These are issues I’ve thought about my whole life. As a native resident of Silicon Valley and the child of two public transit employees, I’ve been acquainted with many of these networks since childhood, and 
                                      I’m very interested in the policies that help people across my beloved home region connect, commute, travel, and explore. At Harvard, I’ve pursued urban studies through both the Government and History of 
                                      Art and Architecture departments to learn more about how urban infrastructure allows people to live, work, and thrive."),
                                    p("These data have been collected from several sources, including the Metropolitan Transportation Commission and the US Census Bureau via Wikipedia. The MTC’s data includes all the major Bay Area transit systems, 
                                      including VTA, Muni, BART, and CalTrain. For questions, please reach out at ecalivo@college.harvard.edu.")
                                    ),
                           tabPanel("Visualization",
                                    h2("Interactive Map of Bay Area Transit Stops"),
                                    p("This map allows you to examine the 23,000+ rapid transit stops across the Bay Area, with overlays to show you municipal boundaries. 
                                       Zoom in to see exact stop locations."),
                                    leafletOutput("map", height = 600)
                           ),
                           tabPanel("Graphic Data",
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
                                      mainPanel(plotlyOutput("plots"))), 
                                    tags$br(),
                                    p("These graphs allow us to look at transit access and income from several different angles. First, note that I decided to account for population and alter the data slightly for the adjusted graphs: since it seems 
                                      clear that the largest cities in the Bay Area would have the most transit stops by virtue of their population and centrality, I decided to divide all raw stop counts by their municipal populations in order to get a 
                                      more representative view. However, I also included the non-adjusted data, which can be examined by choosing it in the drop down menu."),
                                    p("I further allow for us to look at the data by county by checking the box. If we leave the box unchecked and look at data for the Bay Area as a whole, we can see that by both the adjusted and non-adjusted metrics, there 
                                      appears to be a negative correlation between transit access and median household income; that is to say, as median household income goes up, transit access tends to decline. However, if we look by county, this is not the 
                                      case: each line of best fit has a different slope and thus a different correlation coefficient. This allows us to look at differences by county: for example, when looking at the adjusted model, it is clear that Alameda and Contra 
                                      Costa Counties, which are the traditional industrial bases of the Bay Area, have much starker negative correlations than the other counties, while Santa Clara and San Mateo Counties, the center of Silicon Valley, even has a weak positive correlation. 
                                      (Note that San Francisco is a consolidated city-county and thus has only a single point and no line of best fit.)"))))

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
          ggplot(aes(y = adjusted, x = med_income, color = county_ies_note_2)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_y_log10() +
          theme(legend.position = "right") +
          labs(title = "Adjusted Access to Public Transit Based on Median Household Income",
               subtitle = "Adjustment = total transit stops in a municipality divided by population",
               y = "Adjusted Access to Public Transportation (Log)",
               x = "Median Household Income",
               color = "County") 
        the_adjusted_income_plotly <- ggplotly(the_adjusted_income_plot)
        the_adjusted_income_plotly
      }
      
      else
        
      {
        the_nonadjusted_income_plot <- the_table %>% 
          ggplot(aes(y = n, x = med_income, color = county_ies_note_2)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_y_log10() +
          theme(legend.position = "right") +
          labs(title = "Transit Stop Numbers Based on Median Household Income",
               subtitle = "Not adjusted for population",
               y = "Number of Rapid Transit Stops in a Municipality",
               x = "Median Household Income",
               color = "County")
        the_nonadjusted_income_plotly <- ggplotly(the_nonadjusted_income_plot)
        the_nonadjusted_income_plotly
      }
    }
    
    else {
      if(input$x == "Adjusted") {
        the_adjusted_income_plot_2 <- the_table %>% 
          ggplot(aes(y = adjusted, x = med_income)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_y_log10() +
          labs(title = "Adjusted Access to Public Transit Based on Median Household Income",
               subtitle = "Adjustment = total transit stops in a municipality divided by population",
               y = "Adjusted Access to Public Transportation (Log Scale)",
               x = "Median Household Income") 
        the_adjusted_income_plotly_2 <- ggplotly(the_adjusted_income_plot_2)
        the_adjusted_income_plotly_2
      }
      else
        {
        the_nonadjusted_income_plot <- the_table %>% 
          ggplot(aes(y = n, x = med_income)) +
          geom_point(alpha = 0.5) +
          geom_smooth(method = "lm", se = FALSE) +
          scale_y_log10() +
          labs(title = "Transit Stop Numbers Based on Median Household Income",
               subtitle = "Not adjusted for population",
               y = "Number of Rapid Transit Stops in a Municipality",
               x = "Median Household Income")
        the_nonadjusted_income_plotly <- ggplotly(the_nonadjusted_income_plot)
        the_nonadjusted_income_plotly
      }
     
    }
   
  })
  

}

# Run the application 
shinyApp(ui = ui, server = server)


