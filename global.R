library(shiny)
library(arrow)
library(dplyr)
library(ggplot2)
library(leaflet)

flights <- read_parquet("data/flights.parquet")