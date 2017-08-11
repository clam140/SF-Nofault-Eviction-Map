library(leaflet)
library(shiny)
library(shinythemes)
library(RSocrata)
library(lubridate)

Choices <- c("All", "Owner move in", "Ellis Act withdrawal", "Demolition")

# Unfortunately necessary for updated variables used within UI
evictionRawData <- RSocrata::read.socrata("https://data.sfgov.org/resource/93gi-sfd2.csv")
recentlyUpdatedString <- paste(month.name[month(max(evictionRawData$file_date))], year(max(evictionRawData$file_date)))
recentlyUpdatedDateAgg <- as.Date(paste(sep="", year(max(evictionRawData$file_date)), "-", month(max(evictionRawData$file_date)),"-01"))

shinyUI(
        bootstrapPage(theme = shinytheme("sandstone"),
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("EvictMap", width = "100%", height = "100%"),
        absolutePanel(style = "padding: 20px; opaacity: 75;",
                      top = 25, 
                      left = 75,
                      width = 345,
                      fixed = TRUE,
                      draggable = TRUE, 
                      class = "panel panel-default", 
                      height = "auto",
                      h3("No-Fault Evictions in", br(), "San Francisco 1997-", year(recentlyUpdatedDateAgg), br(), "by Neighborhood", align = "center"),
                      p("Recently, widespread reports show that some SF property owners may be fraudulently claiming no-fault eviction reasons on lower-income renters 
                         to raise prices and find higher-income renters. This app seeks to visualize neighborhoods where this phenomenon may be most prevelant"),
                      p("Read more about no-fault evictions at ", a("https://www.sftu.org/evictions/",href = "https://www.sftu.org/evictions/", target = "_blank")),
                      p("Click on circle markers for address information (link to Google Maps View) and specific eviction filing date."),
                      radioButtons("selectMethod",
                                   label = "Date Selection Method",
                                   choices = list("Range" = 1, "Specific Month" = 2),
                                   selected = 2),
                      conditionalPanel(
                              condition = "input.selectMethod == 1",
                              sliderInput("dateRange", 
                                          label = NULL,
                                          min = as.Date("1997-01-01"),
                                          max = recentlyUpdatedDateAgg,
                                          value = c(as.Date("2012-01-01"), recentlyUpdatedDateAgg),
                                          step = 28,
                                          timeFormat = "%m/%Y",
                                          ticks = F)),
                      conditionalPanel(
                              condition = "input.selectMethod == 2",
                              sliderInput("dateSingle",
                                          label = NULL,
                                          min = as.Date("1997-01-01"),
                                          max = recentlyUpdatedDateAgg,
                                          value = recentlyUpdatedDateAgg,
                                          step = 28,
                                          timeFormat = "%m/%Y",
                                          ticks = F)),
                      br(), 
                      selectInput("reason", 
                                  "Eviction Reason", 
                                  choices = Choices, 
                                  selected = "ALL", 
                                  multiple = F),
                      HTML("<b><a href='https://github.com/clam140/SF-Nofault-Eviction-Map' 
                           target = 'blank'>Source Code</a></b>"),
                      h6("By Caleb Lam. Data is updated through DataSF's API."),
                      h6(paste(sep="", "Most recent dataset update: "), strong(recentlyUpdatedString)) 
                      )
        )
        )
