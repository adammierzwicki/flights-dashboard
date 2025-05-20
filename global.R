library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(arrow)
library(dplyr)
library(DT)
library(ggplot2)
library(scales)
library(leaflet)
library(plotly)
library(ggiraph)
library(shinyjs)

flights <- read_parquet("data/flights.parquet")

flights <- flights %>%
  filter(!is.na(country) & !is.na(name) & !is.na(total) & !is.na(date))
  
