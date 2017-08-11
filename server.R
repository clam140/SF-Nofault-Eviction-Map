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
library(RSocrata)
source("func.R")

shinyServer(function(input,output){
        func <- reactive({unifyFunc(D = if(input$selectMethod == 2){input$dateSingle} else {input$dateRange},
                                    R = input$reason)})

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
                                    label = if(input$selectMethod == 2){sprintf("<strong>Neighborhood:<strong> %s <br/> %g evictions since 1997", 
                                                                                neighPoly.Final$nhood, neighPoly.Final$n) %>% lapply(htmltools::HTML)}
                                    else {sprintf("<String>Neighborhood:<strong> %s <br/> %g evictions in time period",
                                                  neighPoly.Final$nhood, neighPoly.Final$n) %>% lapply(htmltools::HTML)},
                                    highlightOptions = highlightOptions(color = "black", weight = 5)) %>%
                        addCircles(group = "circles",
                                   layerId = evictionData$layerID,
                                   lng = evictionData$long,
                                   lat = evictionData$lat,
                                   color = ~circlePal(evictionData$Reason),
                                   fillColor = ~circlePal(evictionData$Reason),
                                   popup = paste("Address: ", paste(sep = "",
                                                                    "<b><a href='http://www.google.com/maps/place/",
                                                                    gsub(" ","+", evictionData$address),
                                                                    ",+San+Francisco,+CA' target = 'blank'>",
                                                                    evictionData$address,
                                                                    "</a></b>", br(),
                                                                    "Eviction file date: ", format(evictionData$file_date, format = "%b %d, %Y"))),
                                   popupOptions = popupOptions(closeOnClick = TRUE),
                                   opacity = 1,
                                   fillOpacity = 0.5,
                                   radius = 40,
                                   stroke = TRUE,
                                   highlightOptions = highlightOptions(color = "black", 
                                                                       weight = 5, 
                                                                       fillOpacity = 1,
                                                                       opacity = 1,
                                                                       bringToFront = TRUE)) %>%
                        addLegend(layerId = "evictLegend",
                                  title = if(input$selectMethod == 2){paste("\"",reason, "\"", br(), "no-fault evictions", br(), "since 1997", sep = "")}
                                  else{paste(sep = "", "\"",reason, "\"", br(), " no-fault evictions between ", br(), 
                                             format(input$dateRange[1], format = "%b %Y"), " and ", format(input$dateRange[2], format = "%b %Y"))},
                                  pal = polyPal, 
                                  values = c(legendStart,legendCount), 
                                  opacity = 0.7, 
                                  position = "bottomright")
                })
        })
