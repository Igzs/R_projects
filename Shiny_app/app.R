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
library(ggmosaic)
#loading data

data("UCBAdmissions")
df_ucba <- tbl_df(UCBAdmissions)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("UC Berkeley Student Admissions"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            selectInput("dept", "Department : ", 
                        choices=unique(df_ucba["Dept"])),
            hr(),
            helpText("Visual reprentation of admition by gender in the selected department")
        ),
        

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("barplot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$barplot <- renderPlot({
        #filter the dataframe according to the input
          df_plot <- df_ucba %>% filter(Dept==input$dept)
         ggplot(data = df_plot) +
          geom_mosaic(aes(weight= n, x = product(Dept,Admit), fill=Admit)) + facet_grid(Gender~.)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
