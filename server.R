library(shiny)
library(leaflet)
library(plyr)
library(dplyr)
library(magrittr)
library(rgdal)
library(rgeos)
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
                leaflet() %>% 
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
                        leafletProxy("EvictMap", data = neighPoly.Final) %>%
                                clearShapes() %>%
                                removeControl(layerId = "evictLegend") %>%
                                addPolygons(group = "polygons",
                                            stroke = TRUE, 
                                            weight = 1, 
                                            color = "black",
                                            fillColor = ~polyPal(as.numeric(neighPoly.Final$n)), 
                                            opacity = 0.5, 
                                            fillOpacity = 0.5, 
                                            smoothFactor = 0.05,
                                            label = sprintf("<strong>Neighborhood:<strong> %s <br/> %g evictions since 1997", 
                                                            neighPoly.Final$nhood, neighPoly.Final$n) %>% lapply(htmltools::HTML),
                                            highlightOptions = highlightOptions(color = "black", weight = 5)) %>%
                                addCircles(group = "circles",
                                        layerId = evictionData$layerID,
                                        lng = evictionData$long,
                                        lat = evictionData$lat,
                                        color = ~circlePal(evictionData$Reason),
                                        fillColor = ~circlePal(evictionData$Reason),
                                        # label = sprintf("<strong>Address:<strong> %s<br/>",
                                        #                evictionData$Address) %>% 
                                        #         lapply(htmltools::HTML),
                                        #highlightOptions = highlightOptions(color = "black", weight = 5),
                                        popup = paste("Address: ", paste(sep = "",
                                                                         "<b><a href='http://www.google.com/maps/place/",
                                                                         gsub(" ","+", evictionData$Address),
                                                                         ",+San+Francisco,+CA' target = 'blank'>",
                                                                         evictionData$Address,
                                                                         "</a></b>")),
                                        popupOptions = popupOptions(closeOnClick = TRUE),
                                        opacity = 1,
                                        fillOpacity = 0.5,
                                        radius = 40,
                                        stroke = TRUE) %>%
                                addLegend(layerId = "evictLegend",
                                          title = paste(reason, br(), "evictions since 1997", sep = ""),
                                          pal = polyPal, 
                                          values = c(0,legendCount), 
                                          opacity = 0.7, 
                                          position = "bottomright")
                        })
        })






