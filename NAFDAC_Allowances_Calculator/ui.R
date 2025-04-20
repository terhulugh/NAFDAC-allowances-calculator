library(shiny)
library(shinydashboard)
library(shinyjs)
library(purrr)
library(shinythemes)
library(officer)
library(rmarkdown)
library(dplyr)
library(tibble)
library(scales)
library(writexl)
library(countrycode)
library(lubridate)
library(DT)

# Load allowances data
dta_data <- read.csv("nafdac_allowances.csv", stringsAsFactors = FALSE)
rank_name <- unique(dta_data$rank)

# Load airports data
nigeria_airports <- read.csv("nigeria_airpot_cities.csv", stringsAsFactors = FALSE)
airport_cities <- unique(nigeria_airports$AirportCities)

# Load state capitals data
nigeria_state_capitals <- read.csv("nigeria_state_capitals.csv", stringsAsFactors = FALSE)
state_capital <- unique(nigeria_state_capitals$StateCapitals)

# Load country names data
country_name <- countrycode::codelist$country.name.en

# Load supplementary allowance category
supp_category <- c("boarding & lodging", "lodging and cash", "lodging only", "cash only")

# UI using shinydashboard
ui <- dashboardPage(
        dashboardHeader(
                title = div(
                        img(src = "nafdac_logo.jpeg", height = "30px", style = "margin-top: -5px;"), 
                        "NAFDAC Allowances Calculator"
                ),
                titleWidth = 400,
                # Set header color to green
                tags$li(class = "dropdown",
                        tags$style(".main-header {background-color: #4CAF50 !important;}") # Green color
                )
        ),
        
        dashboardSidebar(
                width = 300,
                sidebarMenu(
                        id = "tabs",
                        menuItem("Air Travel Allowance", tabName = "air_travel", icon = icon("plane")),
                        menuItem("Road Travel Allowance", tabName = "road_travel", icon = icon("car")),
                        menuItem("First 28 Days Allowance", tabName = "first_28_days", icon = icon("hotel")),
                        menuItem("Estacode Allowance", tabName = "estacode", icon = icon("passport")),
                        menuItem("Estacode Supplementation", tabName = "estacode_supp", icon = icon("money-bill-wave")),
                        menuItem("Warm Clothing Allowance", tabName = "warm_clothing", icon = icon("temperature-low"))
                ),
                hr(),
                div(style = "padding-left: 10px; padding-right:30px;",
                    h4("System Info", style = "margin-top: 0;"),
                    helpText(icon("user"), "Author: NAFDAC Data Science Team"),
                    helpText(icon("calendar"), format(Sys.Date(), "%B %Y")),
                    actionButton("help_btn", "User Guide", 
                                 icon = icon("question-circle"),
                                 class = "btn-info btn-block")
                )
        ),
        
        dashboardBody(
                useShinyjs(),
                tags$head(
                        tags$style(HTML("
        .box {
          border-top: 3px solid #3c8dbc !important;
        }
        .box-header {
          padding: 10px;
        }
        .shiny-output-error-validation {
          color: #ff0000;
          font-weight: bold;
        }
        .form-group {
          margin-bottom: 15px;
        }
      "))
                ),
                
                tabItems(
                        # Air Travel Tab
                        tabItem(
                                tabName = "air_travel",
                                fluidRow(
                                        box(
                                                title = "Air Travel Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_air", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_air", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_air", "Traveling From", choices = c("Select city..." = "", airport_cities)),
                                                selectInput("travel_to_air", "Traveling To", choices = c("Select city..." = "", airport_cities)),
                                                dateInput("start_date_air", "Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_air", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("air_ticket_value", "Air Ticket Value (₦)", value = NULL, min = 0),
                                                actionButton("calculate_air", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_air", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("air_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_air", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_air", "Download CSV", class = "btn-info")
                                        )
                                )
                        ),
                        
                        # Road Travel Tab
                        tabItem(
                                tabName = "road_travel",
                                fluidRow(
                                        box(
                                                title = "Road Travel Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_road", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_road", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_road", "Traveling From", choices = c("Select city..." = "", state_capital)),
                                                selectInput("travel_to_road", "Traveling To", choices = c("Select city..." = "", state_capital)),
                                                dateInput("start_date_road", "Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_road", "Program End Date", format = "yyyy-mm-dd"),
                                                actionButton("calculate_road", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_road", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("road_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_road", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_road", "Download CSV", class = "btn-info")
                                        )
                                )
                        ),
                        
                        # First 28 Days Tab
                        tabItem(
                                tabName = "first_28_days",
                                fluidRow(
                                        box(
                                                title = "First 28 Days Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_first", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_first", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_first", "Traveling From", choices = c("Select city..." = "", state_capital)),
                                                selectInput("travel_to_first", "Traveling To", choices = c("Select city..." = "", state_capital)),
                                                actionButton("calculate_first", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_first", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("first_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_first", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_first", "Download CSV", class = "btn-info")
                                        )
                                )
                        ),
                        
                        # Estacode Tab
                        tabItem(
                                tabName = "estacode",
                                fluidRow(
                                        box(
                                                title = "Estacode Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_estacode", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_estacode", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_estacode", "Traveling From", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_estacode", "Traveling To", 
                                                            choices = c("Select country..." = "", country_name)),
                                                dateInput("start_date_estacode", "Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_estacode", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("exchange_rate_estacode", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_estacode", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_estacode", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("estacode_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_estacode", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_estacode", "Download CSV", class = "btn-info")
                                        )
                                )
                        ),
                        
                        # Estacode Supplementation Tab
                        tabItem(
                                tabName = "estacode_supp",
                                fluidRow(
                                        box(
                                                title = "Estacode Supplement Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_estacode_supp", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_estacode_supp", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_estacode_supp", "Traveling From", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_estacode_supp", "Traveling To", 
                                                            choices = c("Select country..." = "", country_name)),
                                                selectInput("estacode_supplement_category", "Supplement Category", choices = supp_category),
                                                dateInput("start_date_estacode_supp", "Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_estacode_supp", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("cash_received", "Cash Received ($)", value = NULL, min = 0),
                                                numericInput("exchange_rate_estacode_supp", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_estacode_supp", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_estacode_supp", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("estacode_supp_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_estacode_supp", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_estacode_supp", "Download CSV", class = "btn-info")
                                        )
                                )
                        ),
                        
                        # Warm Clothing Tab
                        tabItem(
                                tabName = "warm_clothing",
                                fluidRow(
                                        box(
                                                title = "Warm Clothing Input", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_warm_clothing", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_warm_clothing", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_warm_clothing", "Traveling From", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_warm_clothing", "Traveling To", 
                                                            choices = c("Select country..." = "", country_name)),
                                                numericInput("exchange_rate_warm_clothing", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_warm_clothing", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_warm_clothing", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", DTOutput("warm_clothing_allowance_table")),
                                                br(),
                                                downloadButton("downloadWord_warm_clothing", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_warm_clothing", "Download CSV", class = "btn-info")
                                        )
                                )
                        )
                )
        )
)
