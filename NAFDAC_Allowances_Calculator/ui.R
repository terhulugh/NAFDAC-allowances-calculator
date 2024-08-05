# Load the necessary packages
library(shiny)
suppressWarnings(library(shinythemes))
suppressWarnings(library(officer))
library(rmarkdown)
suppressWarnings(library(dplyr))
library(tibble)
suppressWarnings(library(scales))
suppressWarnings(library(writexl))
suppressWarnings(library(countrycode))

rank_name <- c("Chief Executive/Board Members", "Director", "CONRAISS 15(DD)","CONRAISS 14(AD)", "CONRAISS 13(Chief)",
               "CONRAISS 12(Asst Chief)", "CONRAISS 11(Principal)", "CONRAISS 9(Senior)",
               "CONRAISS 8(Officer I)", "CONRAISS 7(Officer II)", "CONRAISS 6","CONRAISS 5",
               "CONRAISS 4", "CONRAISS 3", "CONRAISS 2", "CONRAISS 1")

airport_cities <- c("Abuja", "Lagos", "PortHarcourt", "Kano", "Akure", "Asaba", "Benin", "BirninKebbi", "Calabar", "Enugu", "Gombe", "Ibadan",
                    "Ilorin", "Jos", "Kaduna", "Katsina", "Maiduguri", "Makurdi", "Minna",
                    "Owerri", "Sokoto", "Uyo", "Warri", "Yola")

state.capital <- c("Abakaliki", "Abeokuta", "Abuja", "AdoEkiti", "Akure", "Asaba", "Awka", "Bauchi", "BeninCity",
                   "BirninKebbi", "Calabar", "Damaturu", "Dutse", "Enugu", "Gombe", "Gusau", "Ibadan", "Ikeja", 
                   "Ilorin", "Jalingo", "Jos", "Kaduna", "Kano", "Katsina", "Lafia", "Lokoja", "Maiduguri", "Makurdi",
                   "Minna", "Osogbo", "Owerri", "PortHarcourt", "Sokoto", "Umuahia", "Uyo", "Yenagoa", "Yola")

country_name <- countrycode::codelist$country.name.en

# Define the UI for the application
ui <- navbarPage(
        "NAFDAC Allowance Calculator app",
        theme = shinytheme("cerulean"),
        tabPanel(
                "Air Travel Allowance",
                fluidPage(
                        titlePanel("Calculate Air Travel Allowance"),
                        sidebarLayout(
                                sidebarPanel(
                                        textInput("name_air", "Name of Staff"),
                                        selectInput("rank_air", "Rank", rank_name),
                                        selectInput("travel_from_air", "Traveling from", airport_cities),
                                        selectInput("travel_to_air", "Traveling to", airport_cities),
                                        numericInput("num_days_air", "Number of Days:", value = "", min = 1),
                                        numericInput("air_ticket_value", "Air Ticket Value:", value = ""),
                                        actionButton("calculate_air", "Calculate Allowance"),
                                        actionButton("reset_air", "Reset"),
                                        hr(),
                                        downloadButton("downloadWord_air", "Download Word"),
                                        downloadButton("downloadCSV_air", "Download CSV"),
                                ),
                                mainPanel(
                                        h4("Air Travel Allowance Breakdown:"),
                                        tableOutput("air_allowance_table")
                                )
                        )
                )
        ),
        tabPanel(
                "Road Travel Allowance",
                fluidPage(
                        titlePanel("Calculate Road Travel Allowance"),
                        sidebarLayout(
                                sidebarPanel(
                                        textInput("name_road", "Name of Staff"),
                                        selectInput("rank_road", "Rank", rank_name),
                                        selectInput("travel_from_road", "Traveling from", state.capital),
                                        selectInput("travel_to_road", "Traveling to", state.capital),
                                        numericInput("num_days_road", "Number of Days:", value = "", min = 1),
                                        actionButton("calculate_road", "Calculate Allowance"),
                                        actionButton("reset_road", "Reset"),
                                        hr(),
                                        downloadButton("downloadWord_road", "Download Word"),
                                        downloadButton("downloadCSV_road", "Download CSV"),
                                ),
                                mainPanel(
                                        h4("Road Travel Allowance Breakdown:"),
                                        tableOutput("road_allowance_table")
                                )
                        )
                )
        ),
        tabPanel(
                "Estacode Allowance",
                fluidPage(
                        titlePanel("Calculate Estacode Allowance"),
                        sidebarLayout(
                                sidebarPanel(
                                        textInput("name_estacode", "Name of Staff"),
                                        selectInput("rank_estacode", "Rank", rank_name),
                                        selectInput("travel_from_estacode", "Traveling from", country_name, selected = "Nigeria"),
                                        selectInput("travel_to_estacode", "Traveling to", country_name),
                                        numericInput("num_days_estacode", "Number of Days:", value = "", min = 1),
                                        numericInput("exchange_rate", "Exchange rate(₦/$)", value = ""),
                                        actionButton("calculate_estacode", "Calculate Allowance"),
                                        actionButton("reset_estacode", "Reset"),
                                        hr(),
                                        downloadButton("downloadWord_estacode", "Download Word"),
                                        downloadButton("downloadCSV_estacode", "Download CSV"),
                                ),
                                mainPanel(
                                        h4("Estacode Allowance Breakdown:"),
                                        tableOutput("estacode_allowance_table")
                                        
                                )
                        )
                )
        )
)
