function(input, output, session) {
  
  output$flight_time_series <- renderPlotly({
    
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter((date >= input$date_range[1] & date <= input$date_range[2]) 
             & (country %in% input$country))
    
    grouped_data <- filtered_data %>%
      group_by(date) %>%
      summarise(total = sum(total))
    
    grouped_data$date <- as.Date(grouped_data$date)
    
    p <- grouped_data %>%
      ggplot(aes(x = date, y = total)) +
      geom_line(color = "#397DCC") +
      labs(x = '', y = "Number of Flights") +
      theme_minimal()
    
    ggplotly(p)
    
  })
  
  # output$top_barchart <- renderPlot({
  #   
  #   filtered_data <- flights %>%
  #     mutate(date = as.Date(date)) %>%
  #     filter((date >= input$date_range[1] & date <= input$date_range[2]) 
  #            & (country %in% input$country))
  #   
  #   grouped_data <- filtered_data %>%
  #     group_by(country) %>%
  #     summarise(total = sum(total))
  # 
  #   grouped_data %>%
  #     arrange(desc(total)) %>%
  #     head(10) %>%
  #     ggplot(aes(x = reorder(country, total), y = total)) +
  #     geom_bar(stat = "identity", fill = "#397DCC") +
  #     scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) + 
  #     coord_flip() +
  #     labs(x = '', y = "Number of Flights") +
  #     theme_minimal()
  # })
  
  observeEvent(input$reset_button, {
    updateDateRangeInput(session, "date_range",
                         start = min(flights$date),
                         end = max(flights$date))
    
    updatePickerInput(session, "country",
                      selected = unique(flights$country))
  })
  
  observeEvent(event_data("plotly_click", source = "country_click"), {
    click_data <- event_data("plotly_click", source = "country_click")
    
    if (!is.null(click_data)) {
      clicked_country <- click_data$customdata
      if (!is.null(clicked_country) && clicked_country %in% unique(flights$country)) {
        updatePickerInput(session, "country", selected = clicked_country)
      }
    }
  })
  
  output$barchart_title <- renderUI({
    if (length(input$country) == 1) {
      HTML(paste("Top Airports in", input$country, "by Flights"))
    } else {
      HTML("Top Countries by Flights")
    }
  })
  
  output$top_barchart <- renderPlotly({
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2],
             country %in% input$country)
    
    if (length(input$country) == 1) {
      grouped_data <- filtered_data %>%
        filter(country == input$country[1]) %>%
        group_by(name) %>%
        summarise(total = sum(total)) %>%
        arrange(desc(total)) %>%
        head(10)
      
      p <- grouped_data %>%
        ggplot(aes(x = reorder(name, total), y = total, text = paste("Airport:", name, "<br>Flights:", format(total, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = "#397DCC") +
        coord_flip() +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        labs(x = '', y = "Number of Flights") +
        theme_minimal() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
      
    } else {
      grouped_data <- filtered_data %>%
        group_by(country) %>%
        summarise(total = sum(total)) %>%
        arrange(desc(total)) %>%
        head(10)
      
      p <- grouped_data %>%
        ggplot(aes(x = reorder(country, total),
                   y = total, 
                   customdata = country, 
                   text = paste("Country:", country, "<br>Flights:", format(total, big.mark = ",")))) +
        geom_bar(stat = "identity", fill = "#397DCC") +
        coord_flip() +
        scale_y_continuous(labels = label_number(scale_cut = cut_short_scale())) +
        labs(x = '', y = "Number of Flights") +
        theme_minimal() +
        theme(panel.grid.major.y = element_blank(),
              panel.grid.minor.y = element_blank())
    }
    
    ggplotly(p, source = "country_click", tooltip = "text") %>%
      layout(clickmode = 'event+select', dragmode = FALSE) %>%
      config(
        displayModeBar = FALSE,
        staticPlot = FALSE,
        scrollZoom = FALSE,
        displaylogo = FALSE
      )
  })
  
  
  output$table <- DT::renderDataTable({
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter((date >= input$date_range[1] & date <= input$date_range[2]) 
             & (country %in% input$country))
    
    grouped_data <- filtered_data %>%
      group_by(date, country) %>%
      summarise(total = sum(total))

    grouped_data$date <- as.Date(grouped_data$date)

    DT::datatable(
      grouped_data, extensions = "Buttons", rownames = FALSE,
      options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'), pageLength = 10, autoWidth = TRUE),
      escape = FALSE)
  })
  
  output$map <- renderLeaflet({ 
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter((date >= input$date_range[1] & date <= input$date_range[2]) 
             & (country %in% input$country))
    
    filtered_data %>%
      filter(!is.na(latitude_deg) & !is.na(longitude_deg)) %>%
      group_by(icao_code, latitude_deg, longitude_deg, name) %>%
      summarize(total = sum(total, na.rm = TRUE)) %>%
      ungroup() -> flights_map
    
    bins <- c(0, 5000, 10000, 50000, 100000, 300000, 1000000, Inf)
    radii <- c(2, 4, 6, 8, 10, 15, 20)
    flights_map$radius <- cut(flights_map$total, breaks = bins, labels = radii, right = FALSE)
    
    threshold <- quantile(flights_map$total, 0.98, na.rm = TRUE)
    
    flights_map$radius <- as.numeric(as.character(flights_map$radius))
    leaflet() %>%
      addTiles() %>%
      setView(lng = 15, lat = 55, zoom = 4) %>%
      addCircleMarkers(
        lng = flights_map$longitude_deg, lat = flights_map$latitude_deg,
        radius = flights_map$radius,
        color = ifelse(flights_map$total>= threshold , "#C9384E", "#397DCC"),
        stroke = ifelse(flights_map$total>= threshold , TRUE, FALSE),, 
        fillOpacity = ifelse(flights_map$total>= threshold , 0.5, 0.3),
        popup = paste(
          "<b>", flights_map$name, "</b>",
          "<br>ICAO Code:", flights_map$icao_code,
          "<br>Flight Count:", flights_map$total)
      )
  })
  
  output$flights_box <- renderValueBox({
    
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter(date >= input$date_range[1] & date <= input$date_range[2],
             country %in% input$country)
    
    total_flights <- sum(filtered_data$total)
    
    valueBox(
      value = formatC(total_flights, format = "d", big.mark = ","),
      subtitle = "Total Flights",
      icon = icon("plane"),
      color = "blue"
    )
  })
  
  output$top_10_linegraph <- renderGirafe({
    filtered_data <- flights %>%
      mutate(date = as.Date(date)) %>%
      filter((date >= input$date_range[1] & date <= input$date_range[2])
             & (country %in% input$country))
    
    top_countries <- filtered_data %>%
      group_by(country) %>%
      summarise(total = sum(total)) %>%
      arrange(desc(total)) %>%
      head(10)
    
    grouped_data <- flights %>%
      select(date, country, total) %>%
      filter(country %in% top_countries$country) %>%
      group_by(date, country) %>%
      summarise(total = sum(total)) %>%
      ungroup()
    
    plot <- grouped_data %>%
      ggplot(mapping = aes(
        x = date,
        y = total,
        color = country,
        tooltip = country,
        data_id = country
      )) +
      geom_smooth_interactive(method = "loess", se = FALSE, aes(group = country), span=0.1, hover_nearest = TRUE)
    interactive_plot <- girafe(ggobj = plot)
    
    interactive_plot <- girafe_options(
      interactive_plot,
      opts_hover(css = "stroke:#69B3A2; stroke-width: 3px; transition: all 0.3s ease;"),
      opts_hover_inv("opacity:0.5;filter:saturate(10%);"),
      opts_toolbar(saveaspng = FALSE)
    )
    interactive_plot
    
  })
  
}
