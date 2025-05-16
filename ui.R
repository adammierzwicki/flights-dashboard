fluidPage(
  
  titlePanel("Interactive Time Series: Flights Over Time"),
  
  sidebarLayout(
    sidebarPanel(
      dateRangeInput("date_range",
                     "Select Date Range:",
                     start = min(flights$date),
                     end = max(flights$date),
                     min = min(flights$date),
                     max = max(flights$date))),
    
    mainPanel(
      plotOutput("flight_time_series"),
      leafletOutput("map", width = "100%", height = "700px"),
    )
  )
)
