dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = "Flights Dashboard"),
  
  dashboardSidebar(

    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("plane")),
      menuItem("About", tabName = "about", icon = icon("info-circle"))   
    ),
    tags$div(
      style = "text-align: center; padding: 10px;",
      tags$img(src = "logo.png", width = "80%", style = "border-radius: 10px;")
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
    tags$head(
      tags$style(HTML("
      .main-header .logo {
        font-size: 20px !important;
        font-weight: bold;
      }
    "))
    ),
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width=4,
                       valueBoxOutput("flights_box", width = NULL),
                       box(title = "Flights Table", width = NULL, height = "958px", withSpinner(dataTableOutput("table")))
                ),
                column(width=8,
                       tabBox(title = "Flights Over Time Analysis", width = NULL, height = "600px", 
                              tabPanel("Total Flights Over Time", 
                                        withSpinner(plotlyOutput("flight_time_series", height = "550px", width = "100%"))
                              ),
                              tabPanel(title = uiOutput("top10_title"),
                                       withSpinner(girafeOutput("top_10_linegraph", height = "550px", width = "100%"))
                              )
                       ),
                       box(title = uiOutput("barchart_title"), solidHeader = TRUE, width = NULL,
                           withSpinner(plotlyOutput("top_barchart")))
                )
              ),
              fluidRow(
                column(width=12,
                       box(title = "Flights Map", width = NULL, solidHeader = TRUE,
                           withSpinner(leafletOutput("map", width = "100%", height = "1000px")))
                )
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




