# fluidPage(
#   
#   titlePanel("Interactive Time Series: Flights Over Time"),
#   
#   sidebarLayout(
#     sidebarPanel(
#       dateRangeInput("date_range",
#                      "Select Date Range:",
#                      start = min(flights$date),
#                      end = max(flights$date),
#                      min = min(flights$date),
#                      max = max(flights$date)),
#       pickerInput("country",
#                   "Select Country:",
#                   choices = sort(unique(flights$country)),
#                   selected = unique(flights$country),
#                   multiple = TRUE,
#                   options = list(
#                     `actions-box` = TRUE,
#                     `live-search` = TRUE,
#                     `none-selected-text` = "No country selected"
#                   ))),
#     
#     mainPanel(
#       plotOutput("flight_time_series"),
#       dataTableOutput("table"),
#       leafletOutput("map", width = "50%", height = "500px"),
#       valueBoxOutput("flights_box", width = 4))
#   )
# )

dashboardPage(
  skin = "black",
  
  dashboardHeader(title = "Flights Dashboard"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("plane"))),
    
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
                ))
  ),
  
  dashboardBody(
    tabItems(
      tabItem(tabName = "dashboard",
              fluidRow(
                column(width=4,
                       valueBoxOutput("flights_box", width = NULL),
                       box(title = "Flights Table", width = NULL, dataTableOutput("table"))),
                column(width=8,
                       box(title = "Flights Over Time", width = NULL,
                           plotOutput("flight_time_series")),
                       box(title = "Top 10 Countries by Flights", width = NULL,
                           plotOutput("top_barchart")))),
              fluidRow(
                column(width=12,
                       box(title = "Flights Map", width = NULL,
                           leafletOutput("map", width = "100%", height = "500px")))
              )
      )
    )
  )
)




