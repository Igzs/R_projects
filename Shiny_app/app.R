#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UC Berkeley Student Admissions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("dept", "Department : ", 
                        choices=unique(df_ucba["Dept"])),
            selectInput("gender", "Gender : ",
                        choices=df_ucba["Gender"]),
            hr(),
            helpText("Test")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("Plot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    ucba <- data("UCBAdmissions")
    df_ucba <- tbl_df(ucba)
    output <- renderPlot({
        ggplot(df_ucba, aes(x=input$dept,y=n))
        + geom_bar(stat="identity")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
