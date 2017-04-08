library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(magrittr)
library(rgdal)
library(ggplot2)
library(lubridate)
library(RColorBrewer)
library(htmltools)
source("func.R")

shinyServer(function(input,output){

        func <- reactive({subsetFunc(date = input$date, 
                                     reason = input$reason)})
        
        
        circlePal <- colorFactor(c("green", "blue", "red"), evictionRawData$Reason)

        
        output$EvictMap <- renderLeaflet({

                leaflet(ZIPpolygons) %>% 
                        addProviderTiles(providers$CartoDB.Positron) %>%
                        setView(lat = 37.75816, 
                                lng = -122.47, 
                                zoom = 13) %>%
                        addLegend(pal = circlePal, 
                                  values = evictionRawData$Reason, 
                                  title = "Eviction Reasons", 
                                  position = 'bottomright')

                })

        observe({
                func()
                if (nrow(evictionData) == 0) {
                        leafletProxy("EvictMap", data = ZIPpolygons) %>%
                                clearShapes() %>%
                                removeControl(layerId = "evictLegend") %>%
                                addPolygons(group = "polygons",
                                            stroke = TRUE, 
                                            weight = 1, 
                                            color = "black",
                                            fillColor = ~polyPal(as.numeric(ZIPpolygons$n)), 
                                            opacity = 0.5, 
                                            fillOpacity = 0.5, 
                                            smoothFactor = 0.05,
                                            label = sprintf("<strong>Zip Code:<strong> %s <br/> %g evictions since 2012", 
                                                             ZIPpolygons$zip, ZIPpolygons$n) %>% lapply(htmltools::HTML),
                                            highlightOptions = highlightOptions(color = "black", 
                                                                                weight = 5,
                                                                                bringToFront = TRUE)) %>%
                                addLegend(layerId = "evictLegend",
                                          title = paste(reason, br(), "evictions since 2012", sep = ""),
                                          pal = polyPal, 
                                          values = c(0,legendCount), 
                                          opacity = 0.7,
                                          position = "bottomright")
                } else {
                        leafletProxy("EvictMap", data = ZIPpolygons) %>%
                                clearShapes() %>%
                                removeControl(layerId = "evictLegend") %>%
                                addPolygons(group = "polygons",
                                            stroke = TRUE, 
                                            weight = 1, 
                                            color = "black",
                                            fillColor = ~polyPal(as.numeric(ZIPpolygons$n)), 
                                            opacity = 0.5, 
                                            fillOpacity = 0.5, 
                                            smoothFactor = 0.05,
                                            label = sprintf("<strong>Zip Code:<strong> %s <br/> %g evictions since 2012", 
                                                            ZIPpolygons$zip, ZIPpolygons$n) %>% lapply(htmltools::HTML),
                                            highlightOptions = highlightOptions(color = "black", weight = 5)) %>%
                                addCircles(group = "circles",
                                        layerId = evictionData$layerID,
                                        lng = evictionData$long,
                                        lat = evictionData$lat,
                                        color = ~circlePal(evictionData$Reason),
                                        fillColor = ~circlePal(evictionData$Reason),
                                        label = sprintf("<strong>Address:<strong> %s<br/>Neighborhood: %s",
                                                       evictionData$Address, evictionData$Neighborhoods...Analysis.Boundaries) %>% 
                                                lapply(htmltools::HTML),
                                        highlightOptions = highlightOptions(color = "black", weight = 5),
                                        opacity = 1,
                                        fillOpacity = 0.5,
                                        radius = 40,
                                        stroke = TRUE) %>%
                                addLegend(layerId = "evictLegend",
                                          title = paste(reason, br(), "evictions since 2012", sep = ""),
                                          pal = polyPal, 
                                          values = c(0,legendCount), 
                                          opacity = 0.7, 
                                          position = "bottomright")

                }

                        
        })
})



