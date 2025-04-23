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
library(shinycssloaders)

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
        skin = "green",
        
        dashboardHeader(
                title = div(
                        img(src = "nafdac_logo.jpeg", height = "30px", style = "margin-right: 10px;"),
                        span("NAFDAC TRAVEL ALLOWANCE SYSTEM", 
                             style = "font-weight: bold; font-size: 18px; color: white;")
                ),
                titleWidth = 450
        ),
        
        dashboardSidebar(
                width = 300,
                sidebarMenu(
                        id = "tabs",
                        menuItem("Dashboard", tabName = "dashboard", icon = icon("home")),
                        menuItem("Air Travel", tabName = "air_travel", icon = icon("plane")),
                        menuItem("Road Travel", tabName = "road_travel", icon = icon("car")),
                        menuItem("First 28 Days", tabName = "first_28_days", icon = icon("moon")),
                        menuItem("Estacode", tabName = "estacode", icon = icon("passport")),
                        menuItem("Estacode Supplement", tabName = "estacode_supp", icon = icon("money-bill-wave")),
                        menuItem("Warm Clothing", tabName = "warm_clothing", icon = icon("temperature-low")),
                        hr(),
                        div(style = "padding: 35px;",
                            h4("Quick Links", style = "font-weight: bold; color: #00512E;"),
                            actionButton("help_btn", "User Guide", icon = icon("question-circle"), 
                                         class = "btn-info", style = "width: 100%; margin-bottom: 10px;"),
                            actionButton("contact_btn", "Contact Support", icon = icon("envelope"), 
                                         class = "btn-success", style = "width: 100%;")
                        )
                )
        ),
        
        dashboardBody(
                useShinyjs(),
                tags$head(
                        tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css?family=Roboto"),
                        tags$style(HTML("
        /* Main styling */
        body {
          font-family: 'Roboto', sans-serif;
          background-color: #f9f9f9;
        }
        
        /* Welcome banner */
        .welcome-banner {
          background: linear-gradient(rgba(0,81,46,0.85), url('nafdac_team.jpg');
          background-size: cover;
          background-position: center;
          color: white;
          padding: 60px 20px;
          text-align: center;
          border-radius: 5px;
          margin-bottom: 20px;
          box-shadow: 0 4px 8px rgba(0,0,0,0.1);
        }
        
        /* Quick access cards */
        .quick-access-card {
          padding: 20px;
          text-align: center;
          background: white;
          border-radius: 5px;
          height: 180px;
          display: flex;
          flex-direction: column;
          justify-content: center;
          align-items: center;
          transition: all 0.3s ease;
          border: none;
          box-shadow: 0 2px 4px rgba(0,0,0,0.05);
          margin-bottom: 20px;
        }
        
        .quick-access-card:hover {
          transform: translateY(-5px);
          box-shadow: 0 6px 12px rgba(0,0,0,0.1);
        }
        
        .quick-access-card img {
          height: 60px;
          margin-bottom: 15px;
        }
        
        .quick-access-card h4 {
          color: #00512E;
          font-weight: bold;
          margin-bottom: 15px;
        }
        
        /* Box styling */
        .box {
          border-top: 3px solid #00512E !important;
          border-radius: 5px !important;
        }
        
        .box-header {
          background-color: #f8f9fa !important;
          border-bottom: 1px solid #e3e6f0 !important;
          font-weight: bold !important;
        }
        
        /* Buttons */
        .btn-primary {
          background-color: #00512E;
          border-color: #00512E;
        }
        
        .btn-primary:hover {
          background-color: #003d20;
          border-color: #003d20;
        }
        
        /* Data table */
        .dataTables_wrapper {
          border-radius: 5px;
        }
      "))
                ),
                
                tabItems(
                        # Dashboard Tab - Welcome Page
                        tabItem(
                                tabName = "dashboard",
                                fluidRow(
                                        # Welcome Banner
                                        box(
                                                width = 12,
                                                status = NULL,
                                                solidHeader = FALSE,
                                                div(class = "welcome-banner",
                                                    img(src = "nafdac_banner.jpeg", height = "80px", 
                                                        style = "margin-bottom: 20px;"),
                                                    h3("Welcome to NAFDAC Travel Allowance Calculation System", 
                                                       style = "font-weight: bold; text-shadow: 1px 1px 3px rgba(0,0,0,0.3);"),
                                                    p("Efficiently calculate travel allowances for official NAFDAC assignments",
                                                      style = "font-size: 18px; margin-bottom: 30px;"),
                                                )
                                        )
                                ),
                                
                                # Quick Access Section
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = span(icon("bolt"), "Quick Access to Calculators"),
                                                status = "primary",
                                                solidHeader = TRUE,
                                                fluidRow(
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("Air Travel"),
                                                                   actionLink("nav_air", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               )),
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("Road Travel"),
                                                                   actionLink("nav_road", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               )),
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("First 28 Days"),
                                                                   actionLink("nav_first28", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               )),
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("Estacode"),
                                                                   actionLink("nav_estacode", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               )),
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("Estacode Supplement"),
                                                                   actionLink("nav_estacode_supp", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               )),
                                                        column(3,
                                                               div(class = "quick-access-card",
                                                                   h4("Warm Clothing"),
                                                                   actionLink("nav_warm_clothing", "Calculate Now", 
                                                                              icon = icon("arrow-right"))
                                                               ))
                                                )
                                        )
                                ),
                                
                                # Help Section
                                fluidRow(
                                        box(
                                                width = 12,
                                                title = span(icon("info-circle"), "Getting Started"),
                                                status = "info",
                                                solidHeader = TRUE,
                                                collapsible = TRUE,
                                                collapsed = TRUE,
                                                p("Welcome to the NAFDAC Travel Allowance Calculation System. To get started:"),
                                                tags$ol(
                                                        tags$li("Select the type of allowance you need to calculate from the sidebar or quick access cards above"),
                                                        tags$li("Fill in the required details in the calculator form"),
                                                        tags$li("Review the calculated allowance breakdown"),
                                                        tags$li("Download or save your calculation as needed")
                                                ),
                                                p("For detailed instructions, click the User Guide button in the sidebar.")
                                        )
                                )
                        ),
                        
                        # Air Travel Tab
                        tabItem(
                                tabName = "air_travel",
                                fluidRow(
                                        box(
                                                title = "Air Travel Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_air", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_air", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectizeInput("travel_from_air", "Departure City", choices = c("Select..." = "", airport_cities)),
                                                selectizeInput("travel_to_air", "Destination City", choices = c("Select..." = "", airport_cities)),
                                                dateInput("start_date_air", "Program Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_air", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("air_ticket_value", "Air Ticket Value ??? (₦)", value = NULL, min = 0),
                                                actionButton("calculate_air", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_air", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("air_allowance_table"))),
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
                                                title = "Road Travel Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_road", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_road", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_road", "Departure City", choices = c("Select city..." = "", state_capital)),
                                                selectInput("travel_to_road", "Destination City", choices = c("Select city..." = "", state_capital)),
                                                dateInput("start_date_road", "Program Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_road", "Program End Date", format = "yyyy-mm-dd"),
                                                actionButton("calculate_road", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_road", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("road_allowance_table"))),
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
                                                title = "First 28 Days Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_first", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_first", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_first", "Departure City", choices = c("Select city..." = "", state_capital)),
                                                selectInput("travel_to_first", "Destination City", choices = c("Select city..." = "", state_capital)),
                                                actionButton("calculate_first", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_first", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("first_allowance_table"))),
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
                                                title = "Estacode Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_estacode", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_estacode", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_estacode", "Departure Country", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_estacode", "Destination Country", 
                                                            choices = c("Select country..." = "", country_name)),
                                                dateInput("start_date_estacode", "Program Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_estacode", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("exchange_rate_estacode", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_estacode", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_estacode", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("estacode_allowance_table"))),
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
                                                title = "Estacode Supplement Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_estacode_supp", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_estacode_supp", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_estacode_supp", "Departure Country", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_estacode_supp", "Destination Country", 
                                                            choices = c("Select country..." = "", country_name)),
                                                selectInput("estacode_supplement_category", "Supplement Category", choices = supp_category),
                                                dateInput("start_date_estacode_supp", "Program Arrival Date", format = "yyyy-mm-dd"),
                                                dateInput("end_date_estacode_supp", "Program End Date", format = "yyyy-mm-dd"),
                                                numericInput("cash_received", "Cash Received ($)", value = NULL, min = 0),
                                                numericInput("exchange_rate_estacode_supp", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_estacode_supp", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_estacode_supp", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("estacode_supp_allowance_table"))),
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
                                                title = "Warm Clothing Allowance", width = 4, status = "primary", solidHeader = TRUE,
                                                textInput("name_warm_clothing", "Name of Staff", placeholder = "Enter staff name"),
                                                selectInput("rank_warm_clothing", "Rank", choices = c("Select rank..." = "", rank_name)),
                                                selectInput("travel_from_warm_clothing", "Departure Country", 
                                                            choices = country_name, selected = "Nigeria"),
                                                selectInput("travel_to_warm_clothing", "Destination Country", 
                                                            choices = c("Select country..." = "", country_name)),
                                                numericInput("exchange_rate_warm_clothing", "Exchange Rate (₦/$)", value = NULL, min = 0),
                                                actionButton("calculate_warm_clothing", "Calculate Allowance", class = "btn-primary"),
                                                actionButton("reset_warm_clothing", "Reset", class = "btn-warning")
                                        ),
                                        
                                        box(
                                                title = "Allowance Breakdown", width = 8, status = "success", solidHeader = TRUE,
                                                div(style = "overflow-x: auto;", withSpinner(DTOutput("warm_clothing_allowance_table"))),
                                                br(),
                                                downloadButton("downloadWord_warm_clothing", "Download Word Report", class = "btn-success"),
                                                downloadButton("downloadCSV_warm_clothing", "Download CSV", class = "btn-info")
                                        )
                                )
                        )
                )
        )
)
