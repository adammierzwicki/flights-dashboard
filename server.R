function(input, output, session) {
  
  output$flight_time_series <- renderPlot({
    
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    grouped_data <- filtered_data %>%
      group_by(date) %>%
      summarise(total = sum(total))
    
    grouped_data$date <- as.Date(grouped_data$date)
    
    grouped_data %>%
      ggplot(aes(x = date, y = total)) +
      geom_line(color = "blue") +
      labs(title = "Flights Over Time",
           x = "Date",
           y = "Number of Flights") +
      theme_minimal()
    
    
  })
  output$flight_time <- renderTable({
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    grouped_data <- filtered_data %>%
      group_by(date) %>%
      summarise(total = sum(total))
    
    grouped_data$date <- as.Date(grouped_data$date)
    
    grouped_data
  })
  output$map <- renderLeaflet({ 
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2])
    
    filtered_data %>%
      filter(!is.na(latitude_deg) & !is.na(longitude_deg)) %>%
      group_by(icao_code, latitude_deg, longitude_deg, name) %>%
      summarize(total = sum(total, na.rm = TRUE)) %>%
      ungroup() -> flights_map
    
    bins <- c(0, 5000, 10000, 50000, 100000, 300000, 1000000, Inf)
    radii <- c(2, 4, 6, 8, 10, 15, 20)
    flights_map$radius <- cut(flights_map$total, breaks = bins, labels = radii, right = FALSE)
    
    flights_map$radius <- as.numeric(as.character(flights_map$radius))
    ifelse(flights_map$radius==20 , "red", "blue")
    leaflet() %>%
      addTiles() %>%
      setView(lng = 15, lat = 55, zoom = 4) %>%
      addCircleMarkers(
        lng = flights_map$longitude_deg, lat = flights_map$latitude_deg,
        radius = flights_map$radius,
        color=ifelse(flights_map$radius>=20 , "red", "blue"),
        stroke =  ifelse(flights_map$radius >= 20 , TRUE, FALSE),, 
        fillOpacity = ifelse(flights_map$radius >= 15 , 0.5, 0.3),
        popup = paste(
          "<b>", flights_map$name, "</b>",
          "<br>ICAO Code:", flights_map$icao_code,
          "<br>Flight Count:", flights_map$total)
      )
  })
}
