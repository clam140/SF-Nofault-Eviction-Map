library(leaflet)
library(shiny)
library(shinythemes)

Choices <- c(
        "All",
        "Owner move in",
        "Ellis Act withdrawal",
        "Demolition")

shinyUI(
        bootstrapPage(theme = shinytheme("sandstone"),
        tags$style(type = "text/css", "html, body {width:100%;height:100%}"),
        leafletOutput("EvictMap", width = "100%", height = "100%"),
        absolutePanel(style = "padding: 20px; opaacity: 75;",
                      top = 170, 
                      left = 75, 
                      fixed = TRUE,
                      draggable = TRUE, 
                      class = "panel panel-default", 
                      height = "auto",
                      h3("No-Fault Evictions in", br(), "San Francisco 2012-2016", align = "center"),
                      sliderInput("date",
                                  "File Date",
                                  min = as.Date("2012-01-01"),
                                  max = as.Date("2016-09-01"),
                                  value = as.Date("2014-01-01"),
                                  step = 28,
                                  timeFormat = "%m/%Y",
                                  ticks = F),
                                  # animate = animationOptions(interval = 200)), 
                      br(), 
                      selectInput("reason", 
                                  "Eviction Reason", 
                                  choices = Choices, 
                                  selected = "ALL", 
                                  multiple = F),
                      helpText("By Caleb Lam. Still in development.")
                      )
        )
        )