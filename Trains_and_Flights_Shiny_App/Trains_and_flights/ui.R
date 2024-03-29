#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(leaflet)
library(maps)
library(htmlwidgets) # To save the map as a web page.
# Define UI for application that draws a histogram
ui <- fluidPage(
  #load library used for disabling ui inputs
  shinyjs::useShinyjs(),
  #
  tags$style(type="text/css","recalculating {opacity: 1.0;}"),
  
  navbarPage("Data Analytics Project",
             
             #Documentation tab
             tabPanel("Documentation",
                      h1("Data Analytics Final Project"),
                      p("Welcome to the Trains and Flights Shiny App"),
                      h2("Goal of the project:"),
                      p("The goal of this project is to construct dashboards displaying information about SNCF rail trafic and US flights.",br(),
                        "This project is the synthesis of every concept reviewed during this Data Analytics course."), 
                      h2("How to use this shiny application:"),
                      p("Navigate through the application using the navigation bar.",br(),
                        "Interact with the different visualizations using the graphical interface.",br(),
                        "In the SNCF dashboard, you can aggregate by year or all departure stations. You can finely tune the aggregation by year by selecting the checkbox and selecting a departure station."),
                      h2("About the datasets:"),
                      h3("SNCF rail trafic details"),
                      p("The dataset associated contains aggregated information about rail trafic accross 4 years ",br(),
                        " At issue is whether the data show signs of improvement or degradation of the state of french rail trafic.",br(),
                        " There were 5462 observations accross 59 train stations. Information about departure and arrival delays, delay causes and average duration were recorded for all thoses lines",br()
                      ),
                      h3("US flights details"),
                      p("Three datasets were used to construct this dashboard:",br()),
                      tags$ol(
                        tags$li("Airlines: Maps IATA flight codes to a specific airline."),
                        tags$li("Airports: Maps IATA flight codes to airports geographical information"),
                        tags$li("Flights: Contains an overview of all the information about the flights")
                      ),
                      p("The combination of these datasets allows us to have an overview of the state of US air trafic",br(),
                        "Comparisons can be drawn between the two dashboards to reveal interesting results "
                      )
                      
             ),
             tabPanel("SNCF dashboard",
                      # Sidebar with a slider input for number of bins 
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons("choice", "Choose which feature to aggregate on:",
                                       c("Year" = "year",
                                         "All departure Stations" = "departure_station")),
                          hr(),
                          checkboxInput('is_departure','Select a departure station?',value = TRUE),
                          uiOutput("select_departure_ui"),
                          radioButtons("display","Choose which information to display:",
                                       c("Overall" = "overall",
                                         "Delay" = "delay",
                                         "Percentages" = "perc")),
                          
                          width=2
                        ),
                        
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          conditionalPanel('input.choice=="year" & input.is_departure==0',titlePanel("SNCF dashboard for all years")),
                          conditionalPanel('input.choice=="departure_station"',titlePanel("SNCF dashboard for all departure stations")),
                          conditionalPanel('input.is_departure==1',titlePanel(textOutput("station_text"))),
                          
                          conditionalPanel('input.display=="overall"',
                                           fluidRow(
                                             splitLayout(cellWidths = c("50%", "50%"), 
                                                         plotOutput("carried_bplot",height = 600), 
                                                         plotOutput("canceled_bplot",height = 600)
                                             ),
                                             splitLayout(cellWidths = c("50%","50%"),
                                                         plotOutput('delay_bplot',height = 600),
                                                         plotOutput('total_time_bplot',height = 600)
                                             )
                                           )
                          ),
                          
                          fluidRow(
                            conditionalPanel('input.display=="delay"',
                                             plotOutput('avg_delay_bplot',height = 600), 
                                             plotOutput('avg_time_lplot',height = 600),
                                             plotOutput('journey_time_delay_plot', height=600)
                            )
                            
                          ),
                          conditionalPanel('input.display == "perc"',
                                           plotOutput('per_canceled_plot',height = 600),
                                           plotOutput('per_causes_bplot',height = 600)
                          )
                          
                          
                        )
                      )
                      
             ),
             tabPanel("Flights dashboard",
                      # Sidebar with a slider input for number of bins
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(
                            "flights_choice",
                            "Choose feature to aggregate on:",
                            c("Airline" = "AIRLINE",
                              "Airports" = "ORIGIN_AIRPORT")
                          ),
                          hr(),
                          width = 2
                        ),
                        
                        # Show a plot of the generated distribution
                        mainPanel(
                          plotOutput('nb_flights_plot'),
                          plotOutput('del_flights_plot'),
                          plotOutput('avg_dur_flights_plot'),
                          plotOutput('avg_dist_flights_plot'),
                          plotOutput('tot_dist_flights_plot'),
                          plotOutput('avg_dep_del_flights_plot'),
                          plotOutput('del_ar_del_flights_plot'),
                          leafletOutput('airports_map')
                        )
                      ))
  )
  
)