library(sf)
library(fs)
library(shiny)
library(rgdal)
library(leaflet)
library(GISTools)
library(tidyverse)

dir.create("raw-data")

download.file("https://opendata.arcgis.com/datasets/a719f8d4c8964b2ba87ecc2bab144568_0.csv?outSR=%7B%22latestWkid%22%3A4326%2C%22wkid%22%3A4326%7D", 
              destfile = "raw-data/transit_stops")
download.file("https://opendata.arcgis.com/datasets/4b1242e5cb224a2c9043927d3344df5a_0.csv?outSR=%7B%22latestWkid%22%3A4326%2C%22wkid%22%3A4326%7D",
              destfile = "raw-data/polygon_table")
download.file("https://opendata.arcgis.com/datasets/4b1242e5cb224a2c9043927d3344df5a_0.zip?outSR=%7B%22latestWkid%22%3A4326%2C%22wkid%22%3A4326%7D",
              destfile = "juris_zip")

transit_geojson <- "https://opendata.arcgis.com/datasets/a719f8d4c8964b2ba87ecc2bab144568_0.geojson"

transit_clean <- readOGR(dsn = transit_geojson)

unzip("juris_zip")

juris_bay <- readOGR("San_Francisco_Bay_Region_Jurisdictions_Incorporated_Places_and_Unincorporated_County_Lands.shp") 

polygon_table <- read_csv("raw-data/polygon_table")

all_transit_stops <- read_csv("raw-data/transit_stops") %>% 
  drop_na(sp_citynam) 

the_map <- leaflet() %>% 
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

the_map

getwd()