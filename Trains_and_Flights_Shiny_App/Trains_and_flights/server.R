#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(scales)
library(ggthemes)
# Define server logic required to draw a histogram
shinyServer(function(input, output) {
    full_trains_df <- read.csv("datasets/full_trains.csv")
    output$testplot <- renderPlot({
        choice = sym(input$choice)
        # generate bins based on input$bins from ui.R
        test_df <- full_trains_df %>% group_by(!!(choice)) %>% summarize(total = sum(total_num_trips) - sum(num_of_canceled_trains))
        print(test_df)
        ggplot(test_df,aes(x=!!(choice),y=total)) +
          geom_bar(stat="identity",width = 0.5,fill="darkblue") +
          scale_y_continuous(labels = comma) +
          scale_fill_manual(values=c("#56B4E9")) +
          theme(axis.text.x = element_text(angle = 45, hjust = 1))

    })

})
