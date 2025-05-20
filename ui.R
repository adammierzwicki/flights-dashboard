dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Flights Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("plane")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))   
    ),
    
    dateRangeInput("date_range",
                   "Select Date Range:",
                   start = min(flights$date),
                   end = max(flights$date),
                   min = min(flights$date),
                   max = max(flights$date)),
    
    pickerInput("country",
                "Select Country:",
                choices = sort(unique(flights$country)),
                selected = unique(flights$country),
                multiple = TRUE,
                options = list(
                  `actions-box` = TRUE,
                  `live-search` = TRUE,
                  `none-selected-text` = "No country selected"
                )),
    actionButton("reset_button",
                 "Reset Filters",
                 icon = icon("undo"))
    
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width=4,
                       valueBoxOutput("flights_box", width = NULL),
                       box(title = "Flights Table", width = NULL, dataTableOutput("table"))
                       ),
                column(width=8,
                       # tabBox(
                       #   title = "First tabBox",
                       #   # The id lets us use input$tabset1 on the server to find the current tab
                       #   id = "tabset1", height = "250px",
                       #   tabPanel("Tab1", "First tab content"),
                       #   tabPanel("Tab2", "Tab content 2")
                       # ),
                       # tabBox(
                       #   title = "First tabBox",
                       #   # The id lets us use input$tabset1 on the server to find the current tab
                       #   id = "tabset1", height = "250px",
                       #   tabPanel("Tab1", "First tab content"),
                       #   tabPanel("Tab2", "Tab content 2")
                       # ),
                       # tabBox(
                       #   side = "right", height = "250px",
                       #   selected = "Tab3",
                       #   tabPanel("Tab1", "Tab content 1"),
                       #   tabPanel("Tab2", "Tab content 2"),
                       #   tabPanel("Tab3", "Note that when side=right, the tab order is reversed.")
                       # ),
                       box(title = "Flights Over Time", width = NULL,
                           plotlyOutput("flight_time_series")),
                       box(title = uiOutput("barchart_title"), solidHeader = TRUE, width = NULL,
                           plotlyOutput("top_barchart"))
                       )
                ),
              fluidRow(
                column(width=12,
                       box(title = "Flights Map", width = NULL,
                           leafletOutput("map", width = "100%", height = "500px")))
              )
      ),
      tabItem(tabName = "about",
              h1("About this Dashboard"),
              p("This dashboard provides an interactive visualization of flight data over time.", br(),
                "You can filter the data by date range and country to see how flight patterns change.", br(),
                "The map shows number of total operations for selected airports, and the table provides detailed information about each flight."),
              br(),
              h1("Data Source"),
              p("Based on ", a("European Flights Dataset", href="https://www.kaggle.com/datasets/umerhaddii/european-flights-dataset?resource=download"), "along with ", a("Airports geospatial data", href="https://ourairports.com/data/"), "."),
      )
    )
  )
)




