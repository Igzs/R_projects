#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(navbarPage("Data Analytics Project",
                           
                           #Documentation tab
                           tabPanel("Documentation",
                                    h1("Data Analytics Final Project"),
                                    p("Welcome to the Trains and Flights Shiny App"),
                                    h2("Goal of the project:"),
                                    p("The goal of this project is to construct dashboards displaying information about SNCF rail trafic and US flights.",br(),
                                      "This project is the synthesis of every concept reviewed during this Data Analytics course."), 
                                    h2("How to use this shiny application:"),
                                    p("Navigate through the application using the navigation bar.",br(),
                                      "Interact with the different visualizations using the graphical interface."),
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
                                    
                                    fluidRow(column(width=2,
                                                    wellPanel(
                                                      radioButtons("choice", "Choose feature to aggregate on:",
                                                                   c("Year" = "year",
                                                                     "Departure Station" = "departure_station")
                                                      )
                                                    )
                                                ),
                                             column(width=8,
                                                    plotOutput('carried_bplot',height=600)
                                             )
                                                    
                                    ),
                                    
                  
                                    fluidRow(column(width=8),
                                             plotOutput('multiple_bplot',height=600)
                                    )
                                  )
                           )
)
