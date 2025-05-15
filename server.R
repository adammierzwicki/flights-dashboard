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
}
